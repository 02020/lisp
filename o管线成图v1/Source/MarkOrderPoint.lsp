

;;���Ʊ��
(defun MarkOrderPoint4Arrows ()
 
  (DelAllBolck "0-Id" "080_A")
  (setq nodeList (MarkOrderPointArrows))
  (ShowBlock_Oh nodeList "0.2")
)

(defun MarkOrderPointArrows (/ en lstPtN ssPl lstss tf)
  (setvar "osmode" 0)
  (princ (strcat "\no__o ��һ��˳���ʾ������"))
  (setq ssPl (ssget "x" (list (cons 0 "LWPOLYLINE") (cons 8 "080_A"))))
  (if (null ssPl)
    (princ "\no__o ������ ����")
    (progn
      (princ (strcat "\no__o ��ȡ��ͼ�θ���:"
		     (itoa (sslength ssPl))
	     )
      )
      (setq n1 -1
	    lstss nil
      )
      (while (setq ssna (ssname ssPl (setq n1 (1+ n1))))
	(setq lstss (cons ssna lstss))
      )
      (setq PlCo
	     (mapcar
	       '(lambda	(x)
		  (setq plObj (vlax-ename->vla-object x))
		  (list
		    x
		    (vlax-curve-getStartPoint plObj)
		    (vlax-curve-getEndPoint plObj)
		  )
		)
	       lstss
	     )
      )
      ;; PlCo ������Ϊ (ͼԪ ��� �յ�)
      (Arrows2PlColst PlCo nil)
    )
  )
)



;;������չʾ��dwg��
(defun ShowBlock_Oh (lstPtN blockSize)
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)

  (setq n2 -1)
  (command "UNDO" "BE")
  (setvar "CLAYER" "080_A")
  (setq samePoint (GetSamePointFromList lstPtN 3))
  (if samePoint
    ;;�������
    (progn
      (setq strError (strcat "\no__o ���ڴ���Ľڵ������" (rtos (length samePoint) 2 0)))
      (princ strError)
      (alert strError)
      (foreach pt samePoint
	(AddCircle pt configPointCircle configLayerTemp)
      )
    )
    (progn
      (setq strqz (strcase (getstring "\no__o ��������ǰ׺:")))
      (while (setq pt1 (nth (setq n2 (1+ n2)) lstPtN))
	(setq str (strcat strqz (rtos (1+ n2) 2 0))
	 block (InsertBlockAttr pt1 blockSize "0-Id" (list str "_" "_" ""))
	      )
        (vla-put-Color block 251)
      )
    )
  )
  (command "UNDO" "E")
)


;;����ͼԪ�ļ��ϻ�ȡ��λ����
(defun GetZoomWindow (PlCo)
  (setq
    lstpt (apply 'append
		 (mapcar
		   '(lambda (x)
		      (list (list (car (cadr x)) (cadr (cadr x)))
			    (list (car (caddr x)) (cadr (caddr x)))
		      )
		    )
		   PlCo
		 )
	  )

    lstpt (list
	    (apply 'mapcar (cons 'min lstpt))
	    (apply 'mapcar (cons 'max lstpt))
	  )
  )
  (vla-ZoomWindow
    (vlex-AcadObject)
    (vlax-3d-point (car lstpt))
    (vlax-3d-point (cadr lstpt))
  )
)




 ;| �ݹ��ж�ԭ�� PlCo ��Ϊ����ݹ� |;
(defun Arrows2PlColst (PlCo lstPtN / plObj PlCoN en PlCoN lstN pln re)
  ;(GetZoomWindow PlCo)
  (setq strP "\no__o ѡ��������ڵ� ��ʼ��ͷ")
  (alert strP)
  (princ strP)
  (setq ssPl1 (ssget ":s" (list (cons 0 "LWPOLYLINE"))))
  (setq en (ssname ssPl1 0))
  (setq plObj (vlax-ename->vla-object en))
  (setq	en     (list en
		     (vlax-curve-getStartPoint plObj)
		     (vlax-curve-getEndPoint plObj)
	       )
	PlCoN  nil
	PlCoN  (cons en PlCoN)
	lstPtN (append lstPtN (cdr en))
  )

  ;;��ȡ����߶���Ϊ�� (Ԫ�� ��� �յ�)
  (setq	PlCo (vl-remove en PlCo)
	pln  (length PlCo)
	re   0
  )

  ;������
  (setq dcl_id (load_dialog "scroll-bar"))
  (new_dialog "scrolling" dcl_id)

  ;;��������ѭ��
  (while (and PlCo (>= pln re))
    ;"ִ�� " (rtos re)
    (set_tile "txt" (strcat  " ���ڴ���ڣ�" (rtos (- pln (length PlCo)))))
    (setq schedule (* 2.0 (/ re 2.0 pln)))
    (scroll-bar schedule)
    
    (setq re (1+ re))
    ;;���������µ�
    (foreach ptt lstPtN
      ;;����PlCo���Ԫ��
      (foreach en PlCo
	(if (member ptt en)
	  (progn
	    (setq PlCo (vl-remove en PlCo))
	    ;;���ptt���Ƕ���ߵ�������ö����
	    (if	(not (equal ptt (cadr en) 0.0001))
	      (setq en (cons (car en) (reverse (cdr en))))
	    )
	    (setq EnN nil)
	    (foreach x PlCoN
	      (if (member ptt x)
		(setq EnN x)
	      )
	    )

	    (setq PlCoN (insertEn (vl-position EnN PlCoN) en PlCoN))
	    (setq lstPtN (insertPt (vl-position ptt lstPtN)
				   (cddr en)
				   lstPtN
			 )
	    )
	  )
	)
	;;end if
      )
    )
  )
  (foreach en PlCoN
    (setq obj (vlax-ename->vla-object (car en)))
    (vla-put-Color obj configArrowColor)
  )
  
  ;������
  (done_dialog)
  (unload_dialog dcl_id)
  
  (if (null PlCo)
    (progn
      (GetUniquePointFromList lstPtN)
    )
    (Arrows2PlColst PlCo lstPtN)
  )
)


;;��ȡΨһ�㣬ȡλ4
(defun GetUniquePointFromList2 (lstPtN)
  (setq	lstN nil
  )
  (mapcar '(lambda (x)
	     (if (not (member x lstN))
		 (setq lstN (cons x lstN)
		 )
	     )
	   )
	  lstPtN
  )
  (reverse lstN)
)
;;��ȡΨһ�㣬ȡλ4
(defun GetUniquePointFromList (lstPtN)
  (setq	lstN nil
	lstT nil
  )
  (mapcar '(lambda (x)
	     (setq _x (mapcar '(lambda (__x) (distof (rtos __x 2 5) 2)) x))
	     (if (not (member _x lstT))
	       (progn
		 (setq lstN (cons x lstN)
		       lstT (cons _x lstT)
		 )
	       )
	     )
	   )
	  lstPtN
  )
  (reverse lstN)
)


;;��ȡ��ͬ�ĵ�
(defun GetSamePointFromList (l1 num / l2)
  (setq
    l1 (mapcar
	 '(lambda (x)
	    (mapcar
	      '(lambda (__x) (distof (rtos __x 2 num) 2))
	      x
	    )
	  )
	 l1
       )
  )

  (while l1
    (setq p1 (car l1)
	  p2 (cdr l1)
    )
    (if	(member p1 p2)
      (setq l2 (append l2 (list p1)))
    )

    (setq l1 (vl-remove p1 l1))
  )
  l2
)











 ;| �ݹ��ж�ԭ�� PlCo ��Ϊ����ݹ� |;
(defun PlCo2PlColst (PlCo lstPtN / plObj PlCoN en PlCoN lstN pln re)
  ;;��ȡ����߶���Ϊ�� (Ԫ�� ��� �յ�)
  (setq	PlCoN  nil
	en     (car PlCo) ;��ȡ��һ��Ԫ��
	PlCo   (cdr PlCo) ;��ȥ��һ��Ԫ��
	PlCoN  (cons en PlCoN)
	lstPtN (append lstPtN (cdr en))
  )
  (setq	pln (length Plco)
	re  0
  )
  ;;��������ѭ��
	  ;(while (and Plco (>= pln re))
	  ;	(setq re (1+ re))
  ;;���������µ�
  (foreach ptt lstPtN
    ;;����PlCo���Ԫ��
    (foreach en	PlCo
      (if (member ptt en)
	(progn
	  (setq PlCo (vl-remove en PlCo))
	  ;;���ptt���Ƕ���ߵ�������ö����
	  (if (not (equal ptt (cadr en) 0.0001))
	    (setq en (cons (car en) (reverse (cdr en))))
	  )
	  (setq EnN nil)
	  (foreach x PlCoN
	    (if	(member ptt x)
	      (setq EnN x)
	    )
	  )
	  (setq PlCoN (insertEn (vl-position EnN PlCoN) en PlCoN))
	  (setq	lstPtN
		 (insertPt (vl-position ptt lstPtN) (cddr en) lstPtN)
	  )
	)
      )
    )
  )
	  ;)
  (if (null PlCo)
    (progn
      (setq lstN nil)
      (mapcar '(lambda (x)
		 (if (not (member x lstN))
		   (setq lstN (cons x lstN))
		 )
	       )
	      lstPtN
      )

      (reverse lstN)
    )
    (PlCo2PlColst PlCo lstPtN)
  )
)

