(defun __________3.�ĵ�����__________ () ())
;;ͼ������


;| ���ݽڵ����ʲ��������Ŀ� |;
(defun InsertPointBlock	(/ strPr)
  (princ "\no__o  ���ݽڵ����ʲ����")
  (cond
    ((null currentLayer)
     (princ "\no__o �����õ�ǰͼ��")
     (alert "�����õ�ǰͼ��")
     (exit)
    )
    ((not (tblsearch "layer" currentLayer))
     (princ (strcat "\no__o " currentLayer " ͼ�㲻����"))
     (alert (strcat currentLayer " ͼ�㲻����"))
     (exit)
    )
    (t (princ (strcat "\no__o ��ǰ�Ĺ���ͼ��Ϊ " currentLayer " ")))
  )
  (setvar "CLAYER" currentLayer)
  (setq blockSize "0.5")
  (setq
    ssOh (ssget	"x"
		(list '(0 . "insert") (cons 2 "0-Id") (cons 8 "080_A"))
	 )
  )
  (if (null ssOh)
    (princ "\no__o ������ �ڵ��")
    (progn
      (setq ssOh (ssnameX ssOh))
      (foreach en ssOh
	(setq
	  en   (cadr en)
	  pt   (cdr (assoc 10 (entget en)))
	  lstA (BlockGetAtts en)
	  attr (cdr (assoc "node" lstA))
	)
	(if (> (strlen attr) 0)
		(if (null (BlokcIsExist attr))
		  (princ (strcat "\no__o " attr "�� ������"))
		  (VL-CMDF "INSERT" attr pt blockSize blockSize "" )
		)
	  )
      )
    )
  )
)



 ;| 2.���� ʸ����ͷ ���� ����� |;
(defun Arrows2Pline (/ ssPl tfD lstss PlCo _pt lstPl)
  (princ "\no__o ���� ʸ����ͷ ���� �����")
  (princ "\no__o ��ѡ���ͷ")
  (setvar "osmode" 0)
  (setq ssPl (ssget (list (cons 0 "LWPOLYLINE") (cons 8 "080_A"))))
  (if (null ssPl)
    (princ "\no__o ͼ�� 080_A ������ ʸ����ͷ")
    (progn
      (princ
	(strcat	"\no__o ��ȡ���� ʸ����ͷ ����Ϊ:"
		(itoa (sslength ssPl))
	)
      )
      (setq tfD t)
    )
  )
  (setq lstss (cdr (reverse (ssnameX ssPl))))
  (setq	PlCo (mapcar '(lambda (x)
			(setq plObj (vlax-ename->vla-object (cadr x)))
			(list
			  (cadr x)
			  (vlax-curve-getStartPoint plObj)
			  (vlax-curve-getEndPoint plObj)
			)
		      )
		     lstss
	     )
  )
  (while PlCo
    (setq _pt (cadr (car PlCo)))
    (while _pt
      ;;����PlCo���Ԫ��
      (setq lstPl nil)
      (foreach en PlCo
	(if (member _pt en)
	  (progn
	    ;;�ж� _pt �ǲ��� en  �����
	    (if	(equal _pt (cadr en) 0.0001)
	      (progn
		(if (null lstPl)
		  (setq lstPl (cons _pt lstPl))
		)
		(setq _pt   (last en)
		      lstPl (cons _pt lstPl)
		      PlCo  (vl-remove en PlCo)
		)
	      )
	    )
	  )
	)
      )
      (setq _pt nil)
      ;;���ƶ����
      (#EntMake_pline
	(list (cons 10 (reverse lstPl))
	      (cons 40 0.2)
	      (cons 8 "080_0")
	)
      )
    )
    ;;end while _pt
  )
  (princ "\no__o ���Ƶ� ����� ��080_0ͼ��")
  (princ)
)





 ;| ��Ohת���ı� |;
(defun c:Oh->Text (/ strPr)
  (princ "\no__o Oh��->�ı� ")
  (setq
    ssOh (ssget	"x"
		(list '(0 . "insert") (cons 2 "0h") (cons 8 "080_A"))
	 )
  )
  (command "UNDO" "BE")
  (setvar "CLAYER" "080_0")
  (if (null ssOh)
    (princ "\no__o ������ �̵߳� ����")
    (progn
      (setq ssOh (ssnameX ssOh))
      (foreach en ssOh
	(setq en (cadr en))
	(setq pt (cdr (assoc 10 (entget (entnext en)))))
	(setq lstA (BlockGetAtts en)
	      ID   (cdr (assoc "ID" lstA))
	)
	(#EntMake_Text ID pt 1 0)

      )	  ;end foreach

      ;;end if
    )
  )
  (command "UNDO" "E")
  (princ)
)



 ;| ����ͼ�� 080_* |;
(defun c:layerFreeze_080 ()
  (princ "\no__o ����ȡ�� 080_0 ͼ���ϵ�ͼԪ")
  (setvar "CLAYER" "0")
  (Layer_Freeze "080_0")
  (Layer_Freeze "080_k")
  (Layer_Freeze "080_A")
)