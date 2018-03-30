;;检测块所有的属性
(defun CheckBlockValueId (lst)
  (setq
    mapList (list (cdr (assoc "h" lst))
		  (cdr (assoc "hc" lst))
		  (cdr (assoc "node" lst))
	    )
  )
  (apply 'or
	 (mapcar
	   '(lambda (x) (or (= x "_") (= x "00.00") (= x "")))
	   mapList
	 )
  )
)

;;检测块所有的属性
(defun CheckBlockAllValue (lst)

  (apply 'or
	 (mapcar
	   '(lambda (x)
	      (setq x (cdr x))
	      (or (= x "_") (= x "00.00") (= x "")))
	   lst
	 )
  )
)

;;孤线检查


 ;| 计算 曲线 长度 |;
(defun SumCurveLen (/ CURVE TLEN SS N SUMLEN)
  (princ "\no__o:计算 曲线 长度")
  (setq sumlen 0)
  (setq
	ss (ssget '((0 . "circle,ellipse,line,*polyline,spline,arc")))
  )
  (setq n 0)
  (repeat (sslength ss)
	(setq curve (vlax-ename->vla-object (ssname ss n)))
	(setq tlen (vlax-curve-getdistatparam
				 curve
				 (vlax-curve-getendparam curve)
			   )
	)
	(setq sumlen (+ sumlen tlen))
	(setq n (1+ n))
  )
  (princ (strcat "\n共选择 "
				 (itoa (sslength ss))
				 " 条线段, 总长: "
				 (rtos sumlen 2 3)
				 "米."
		 )
  )
  (princ)
)


;| 检测箭头是否 0 |;
(defun CheckArrowLen (/ ssna listPt)
  (princ "\no__o 检测是否存在长度为 0 的箭头")
  (setvar "osmode" 0)
  (setq ss (ssget (list '(0 . "LWPOLYLINE") (cons 8 "080_A"))))
  (setq n1 -1 listPt nil)
  (while (setq en (ssname ss (setq n1 (1+ n1))))
    (setq o   (vlax-ename->vla-object en)
	  pts (vlax-curve-getStartPoint o)
	  pte (vlax-curve-getEndPoint o)
    )
    (if	(equal (distance pts pte) 0 0.0001)
      (setq listPt (cons pts listPt)
      )
    )
  )

  (foreach pt listPt
    (entmake (list '(0 . "CIRCLE")
		   (cons 10 pt)
		   '(40 . 2)
		   (cons 8 "080_T")
	     )
    )
  )
  (if (null listPt)
    (princ (strcat "\n检测通过"))
    (princ (strcat "\n__长度为0的多段线个数为:"
		   (itoa (length listPt))
	   )
    )
  )
  (princ)
)

 ;| 检测 Oh块 数据的完整 |;
(defun CheckBlockId (/ strPr)
  (princ "\no__o 检测 节点块 数据的完整 ")
  (setq strPr "")
  (setq
    ssOh (ssget	"x"
		(list '(0 . "insert") (cons 2 "0-Id") (cons 8 "080_A"))
	 )
  )
  (if (null ssOh)
    (princ "\no__o 不存在 高程点 数据")
    (progn
      ;;提取 块带的属性 点坐标 为关键字
      (setq ssOh (ssnameX ssOh))
      (foreach en ssOh
	(setq en (cadr en))
	(setq lstA (BlockGetAtts en)
	      ID   (cdr (assoc "id" lstA))
	)
	(if (CheckBlockAllValue lstA)
	  (setq strPr (strcat strPr ID " "))
	)
      )					;end foreach
      (if (> (strlen strPr) 1)
	(princ (strcat "\no__o 以下节点数据不正确 " strPr))
	(princ "\no__o 检查通过")
      )
      ;;end if
    )
  )
)



 ;| 检测流向与管径 |;
(defun CheckArrow (/      ssPl   ssOh   tfH	   tfPl	  en	 pt
		       lstT   lstAssoc	    plObj  ptS	  ptE	 lst1
		       lst2   ssPl   lstPt  lst	   Ohgc	  strPr
		      )
  (defun lst2strID (lst1 lst2)
    (strcat (cdr (assoc "ID" lst1))
	    "->"
	    (cdr (assoc "ID" lst2))
    )
  )
  (defun pt->str (pt)
    (strcat (rtos (car pt) 2 0) (rtos (cadr pt) 2 0))
  )
  (princ "\no__o 检测高程点与流向")
  (setq ssPl (ssget "x" (list '(0 . "LWPOLYLINE") (cons 8 "080_A"))))
  (setq
    ssOh (ssget	"x"
		(list '(0 . "insert") (cons 2 "0-Id") (cons 8 "080_A"))
	 )
  )
  (if (null ssOh)
    (princ "\no__o 不存在 高程点 数据")
    (setq tfH t)
  )
  (if (null ssPl)
    (princ "\no__o 不存在 箭头 数据")
    (setq tfPl t)
  )
  (setq	lstPt nil
	lstAssoc nil
	strErr ""
	strPr ""
	strDis ""
  )
  (if (and tfH tfPl)
    (progn
      ;;提取 块带的属性 点坐标 为关键字
      (setq ssOh (ssnameX ssOh))
      (foreach en ssOh
	(setq en (cadr en))
	(setq pt (pt->str (cdr (assoc 10 (entget en)))))
	(setq lstT (list (BlockGetAtts en)))
	(setq lstAssoc (cons (cons pt lstT) lstAssoc))
      )
      ;;匹配数据
      (setq lstPt
	     (mapcar
	       '(lambda	(x)
		  (setq plObj (vlax-ename->vla-object (cadr x)))
		  (setq
		    ptS	 (pt->str (vlax-curve-getStartPoint plObj))
		    ptE	 (pt->str (vlax-curve-getEndPoint plObj))
		    lst1 (assoc ptS lstAssoc)
		    lst2 (assoc ptE lstAssoc)
		    str	 (if
			   (setq xdata (assoc -3 (entget (cadr x) '("dia"))))
			    (cdr (cadadr xdata))
			    ""
			 )
		  )
		  (list lst1 lst2 str)
		)
	       (ssnameX ssPl)
	     )
      )					;end setq
      ;;数据输出
      (foreach lst lstPt
	(setq lst1 (cadar lst)
	      lst2 (cadadr lst)
	)
	(if (and (not (null lst1))
		 (not (null lst2))
		 (not (BlockOhValue lst1))
		 (not (BlockOhValue lst2))
	    )
	  (progn
	    (setq Ohgc (- (- (distof (cdr (assoc "h" lst1)))
			     (distof (cdr (assoc "hc" lst1)))
			  )
			  (- (distof (cdr (assoc "h" lst2)))
			     (distof (cdr (assoc "hc" lst2)))
			  )
		       )
	    )				;end setq
	    (if	(< Ohgc 0)
	      (setq strPr (strcat (lst2strID lst1 lst2) "   " strPr))
	    )
	    ;;判断管径是否存在
	    (if	(= (caddr lst) "")
	      (setq strDis (strcat (lst2strID lst1 lst2) "   " strDis))
	    )
	  )
	  (setq strErr (strcat (lst2strID lst1 lst2) "   " strErr))

	)
      )


    )					;end progn
  )
  (cond	((> (strlen strErr) 1)
	 (princ (strcat "\no__o 以下 id块 数据不正确 " strErr " "))
	)
	((> (strlen strPr) 1)
	 (princ (strcat "\no__o 以下矢量箭头不正确 " strPr " "))
	)
	((> (strlen strDis) 1)
	 (princ	(strcat "\no__o 以下矢量箭头 无管径宽度 " strDis " ")
	 )
	)
	(t (princ "\no__o 检查通过"))
  )
  (princ)
)






(defun __________o.其他__________ () ())

 ;| 其他 |;
 ;| 计算高程 |;
(defun c:CalculateHight	(/ ss ssD lstpt lsten lsth)
  (setvar "osmode" 0)
  (setvar "clayer" "080_T")
  (princ
    "\no__o 根据已知高程点计算其他点高程\n从低到高 选取 高程点"
  )
  (setq	lstpt nil
	lsten nil
  )
  (vl-cmdf "PLINE")
  (while (setq ss (ssget ":S" '((0 . "insert") (2 . "G904"))))
    (setq pt (cdr (assoc 10 (entget (ssname ss 0)))))
    (setq lstpt (cons pt lstpt))
    (setq lsten (cons (ssname ss 0) lsten))
    (vl-cmdf pt)
  )
  (vl-cmdf "")
  (setq lstpt (reverse lstpt))
  (setq lsten (reverse lsten))
  (setq	lsth (mapcar '(lambda (x)
			(distof (cdr (assoc "H" (BlockGetAtts x))) 1)
		      )
		     (list (car lsten) (last lsten))
	     )
  )

  (setq ent (entlast))
  (setq	curve-obj (vlax-ename->vla-object ent)
	tlen	  (vlax-curve-getDistAtParam
		    curve-obj
		    (vlax-curve-getEndParam curve-obj)
		  )
  )
  (setq hc (abs (float (- (car lsth) (cadr lsth)))))
  (setq rate (/ hc tlen))

  (setq	lsth
	 (mapcar '(lambda (x)
		    (+ (car lsth)
		       (* rate (vlax-curve-getDistAtPoint curve-obj x))
		    )
		  )
		 lstpt
	 )
  )
					;砍头去尾
					;(setq lsten (reverse (car (reverse (car lsten)))))
  (setq	n1 1
	re (- (length lsten) 2)
  )
  (repeat re
    (BlockSetAtts
      (vlax-ename->vla-object (nth n1 lsten))
      (list "H" (rtos (nth n1 lsth) 2 2))
    )
    (setq n1 (1+ n1))
  )
  (princ)
)






