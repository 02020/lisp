(setq blockSize 0.2)


;;���Ʊ��
(defun GetOrderId ()
  (princ "\no__o ���ɽڵ����")
  (setq lstPt nil
    ssArrow (ssget "x" (list (cons 0 "LWPOLYLINE") (cons 8 "080_A")))
  )
  (setq arrowList (mapcar 'GetArrowIdFromX (ssnameX ssArrow))
        arrowList (vl-sort arrowList
                           (function (lambda (e1 e2) (< (car e1) (car e2))))
                  )
  )
  (foreach en arrowList
    (setq
      id     (car en)
      vlaObj (vlax-ename->vla-object (cadr en))
      pte    (vlax-curve-getEndPoint vlaObj)
      pts    (vlax-curve-getStartPoint vlaObj)

    )
    (setq lstPt (cons pts lstPt))
    (setq lstPt (cons pte lstPt))
  )
  ;;��ת����������ٽ����޳��ظ�
  (setq lstPt    (reverse lstPt)
        nodeList (GetUniquePointFromList lstPt)
  )
  (DeleteBlockIdByColor 251)
  (ShowBlock_Oh nodeList "0.2")
)



;;���������
(defun SetBlockNodeAttr (/)
  (princ "\no__o �������Ը��Ƹ��̵߳�")
  (setq sameset "1") ;ȫ�ֱ���
   (setq  layerName "080_A"
    blockName "0-Id"
	  color 250
	  )
  (setq newStr (getstring "\no__o: ����������ֵ:"))
  (WHILE (and (princ "\nѡ��Ҫ���в��������Կ�")
	      (setq ss (ssget  	(list '(0 . "insert") (cons 62 color) (cons 2 blockName) (cons 8 layerName))))
	 )
    (BlockAttrEdit ss newStr)
  )
)

 ;| �ϲ�0h�� |;
(defun CombineBlockId (/ ssOh lstss lstD lst)
  (princ "\no__o �ϲ���")
  (setq
    ssOh (ssget
	   "x"
	   (list '(0 . "insert") (cons 2 "0-Id") (cons 8 "080_A"))
	 )
  )
  (setq lstss (mapcar '(lambda (x) (cadr x)) (ssnameX ssOh)))
  (setq lstD nil)
  (setq lstG (lstenPt->Group lstss nil))
  (if (apply 'and (mapcar '(lambda (x) (< (length x) 3)) lstG))
    (progn
      (foreach lst lstG
	(setq id ""
	      h1 ""
	      h2 ""
	      node ""
	)
	(setq _pt (cdr (assoc 10 (entget (car lst)))))
	(foreach x lst
	  (setq	lstA  (BlockGetAtts x)
		_id   (cdr (assoc "id" lstA))
		_h1   (cdr (assoc "h" lstA))
		_h2   (cdr (assoc "hc" lstA))
		_node (cdr (assoc "node" lstA))
		h1    (strcat (if (or (= _h1 "00.00") (= _h1 "_"))
				""
				_h1
			      )
			      h1
		      )
		h2    (strcat (if (or (= _h2 "00.00") (= _h22 "_"))
				""
				_h2
			      )
			      h2
		      )
		node  (strcat _node node)
	  )
	  (if (= (strlen _h1) 1) (setq 	id _id))
	)
	(setq
	  id   (vl-string-trim "_" id)
	  h1   (vl-string-trim "_" h1)
	  h2   (vl-string-trim "_" h2)
	  node (vl-string-trim "_" node)
	)
	(setq lstD (cons (list _pt id h1 h2 node) lstD))
      )
      (setq n2 -1)
      (setvar "CLAYER" "080_A")
      (command "UNDO" "BE")
      (while (setq lst (nth (setq n2 (1+ n2)) lstD))
	(VL-CMDF "INSERT"
		 "0-Id"
		 (nth 0 lst)
		 blockSize
		 blockSize
		 ""
		 (nth 1 lst)
		 (nth 2 lst)
		 (nth 3 lst)
		 (nth 4 lst)

	)
      )
      (vl-cmdf "ERASE" ssOh "")
      (command "UNDO" "E")
      (princ "\no__o ok")

    )
    (progn
      (princ "\no__o ͬһ��Ŀ�����<>2, ����ִ�����ʵ��ж�")
      (alert "ͬһ��Ŀ�����<>2\n\n����ִ�� ���ʵ� �ж�")
      (foreach x lstG
	(setq pt (cdr (assoc 10 (entget (car x)))))
	(if (/= (length x) 2)
	  (progn
	    (vla-ZoomCenter
	      (vlax-get-acad-object)
	      (vlax-3d-point pt)
	      12
	    )
	    (prompt "<ESC> ��λ��һ����:")
	    (vl-cmdf pause)
	  )
	)
      )
    )
  )
  (princ)
)

(defun DeleteBlockIdByColor (color)
  ;(initget 7)
  (setq
    layerName "080_A"
    blockName "0-Id"
    ss	      (ssget
		"x"
		(list '(0 . "insert") (cons 62 color) (cons 2 blockName) (cons 8 layerName))
	      )
  )
  (if ss
    (progn
      (setq op (getstring "\no__o ȷ��ִ��:[��(Y)]"))
      (if (or (= op "Y") (= op "y"))
	(command "_ERASE" ss "")
      )
    )
  )
)











 ;|
2.��ȡ�̵߳�(����ʸ����ͷ����Oh��)
ִ�к���Ҫ������ڵ�����
|;
(defun AddBlockH (/ lstpt ssD ssH ss2 lstN strBlock olayerStr)
  (princ "\no__o ��ȡ�̵߳�(����ʸ����ͷ���ƽڵ��)")
   (setvar "CLAYER" "080_A")
  (setq
    layerName "080_A"
    blockName "0-Id"
    blockSize 0.2
   )
  (setq ssPl (ssget "x" (list '(0 . "LWPOLYLINE") (cons 8 "080_A"))))
  (setq ssH (ssget "x" (list '(0 . "insert") (cons 2 "0-Elevation"))))
  (if (null ssH)
    (princ "\no__o ������ �̵߳� ����")
  )
  (if (null ssPl)
    (princ "\no__o ������ ��ͷ ����")
  )
(if (or (null ssH) (null ssPl))   (exit)  )

  (setq
    ssH	  (ssnameX ssH)
    lstH  nil
    lstN  nil
    lstPt (GetCurveListFromX ssPl)
  )
  (foreach en ssH
    (setq en (cadr en))
    (setq pt (cdr (assoc 10 (entget en))))
    (foreach x lstpt
      (if (equal (list (car pt) (cadr pt) 0) x 0.001)    
	(setq lstH (cons en lstH))
      )
    )
  )
  (mapcar
    '(lambda (x)
       (if (not (member x lstN))
	 (setq lstN (cons x lstN))
       )
     )
    lstH
  )

  (DeleteBlockIdByColor 250)
  (foreach en lstN
    (setq pt	(cdr (assoc 10 (entget en)))
	  h1	(cdr (assoc "h" (BlockGetAtts en)))
	  block	(InsertBlockAttr pt blockSize blockName (list "_" h1 "_" ""))
    )
    (vla-put-Color block 250)

  )
  (setq	prstr (strcat "\nʵ�� �̵߳����Ϊ: "
		      (itoa (length (th-list-only lstPt)))
		      "\nĿǰ �̵߳����Ϊ: "
		      (itoa (length lstN))
	      )
  )
  (princ prstr)
  (alert prstr)


  (princ)
)

















;;������ߵ�ͼԪ�б� (start end)
(defun GetCurveListFromX (ss / o)
  (apply 'append
	 (mapcar
	   '(lambda (x)
	      (setq o (vlax-ename->vla-object (cadr x)))
	      (list
		(vlax-curve-getStartPoint o)
		(vlax-curve-getEndPoint o)
	      )
	    )
	   (ssnameX ss)
	 )
  )

)