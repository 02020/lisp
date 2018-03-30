;| 打断多段线の自动选择最近的顶点 |;
(defun PlineAutoBreak (/ os pt1 pt2)
  (princ "\no__o 打断多段线の自动选择最近的顶点 ")
  (setvar "CMDECHO" 0)
  (setq os (getvar "osmode"))
  (setq ssPl (entsel "\no__o选择对象: "))
  (setq pt (cadr ssPl))
  (setq	lstPt (vl-remove-if
				'not
				(mapcar	'(lambda (x)
						   (if (= (car x) 10)
							 (cdr x)
						   )
						 )
						(entget (car ssPl))
				)
			  )
  )
  (setq	lstPt
		 (vl-sort
		   (mapcar '(lambda (x) (list (distance pt x) x)) lstPt)
		   (function (lambda (e1 e2) (< (car e1) (car e2))))
		 )
  )
  (setq pt (cadar lstPt))
  (setvar "osmode" 0)
  (vl-cmdf "break" ssPl "f" pt "@")
  (setvar "osmode" os)
  (princ)
)


 ;| 将连接的 条多段线 合并成一条 多段线 |;
(defun PlineJoin (/ n1 ln1)
  (setvar "osmode" 0)
  (setq ss (ssget))
  (setq n1 -1)
  (vl-cmdf "JOIN")
  (while (setq ln1 (ssname ss (setq n1 (1+ n1))))
	(vl-cmdf ln1)
  )
  (vl-cmdf "")
  (princ)
)

 ;| 将连接的 直接或多条多段线 合并成一条 多段线 |;
(defun PlineJoinline ()
  (princ "\no__o 将连接的 直接或多条多段线 合并成一条 多段线")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))
  (setq	tf nil
		n1 -1
  )
  (while (setq ssna (ssname ss (setq n1 (1+ n1))))
	(setq mode (#DXFS 0 ssna))
	(if	(= (cdr (assoc 0 (entget ssna))) "LINE")
	  (setq tf T)
	)
  )
  (if tf
	(if	(/= (getstring "是否就直线转换成多段线<Y,N>") "N")
	  (progn
		(vl-cmdf "PEDIT" "M" ss "" "Y" "J" "" "")
	  )
	)
	(vl-cmdf "PEDIT" "M" (ssget "P") "" "J" "" "")
  )
  (princ)
)

 ;| 反转 多段线 线 |;
(defun RevPline	(/ SS)
  (princ "\no__o:反转 多段线 线")
  (if (setq ss (ssget '((0 . "LWPOLYLINE"))))
	(progn
	  (setq i 0)
	  (repeat (sslength ss)
		(setq ent (entget (ssname ss i)))
		(foreach n ent
		  (if (= (car n) 10)
			(setq lst_pt (cons n lst_pt))
		  )
		)
		(foreach i '(10)
		  (setq	ent	(vl-remove-if
					  '(lambda (n) (= (car n) i))
					  ent
					)
		  )
		)
		(setq ent (append ent lst_pt))
		(entmod ent)
		(setq i (1+ i))
	  )
	)
	(princ ">>>图内没有二维多段线")
  )
  (princ)
)




 ;| 检测多段线长度是否 >70 |;
(defun CheckPlineLen (/ ssna listPt)
  (princ "\no__o 检测多段线长度是否 >70")
  (setvar "osmode" 0)
  (setq sspls (ssget (list (cons 0 "LWPOLYLINE"))))
  (setq n1 -1)
  (while (setq ssna (ssname sspls (setq n1 (1+ n1))))
	(setq listPl (th2-get-allvertexs ssna))
	(setq n2 0)
	(while (setq pt1 (nth (setq n2 (1+ n2)) listPl))
	  (setq pt2 (nth (- n2 1) listPl))
	  (if (> (distance pt1 pt2) 70)
		(setq listPt (cons pt1 listPt)
			  listPt (cons pt2 listPt)
		)
	  )
	)
  )
  (foreach pt listPt
	(entmake (list '(0 . "CIRCLE")
				   (cons 10 pt)
				   '(40 . 2)
				   (cons 8 "080T")
			 )
	)
  )
  (if (null listPt)
	(princ (strcat "\n不存在长度 >70 的多段线"))
	(princ (strcat "\n__长度 >70 的多段线个数为:"
				   (itoa (/ (length listPt) 2))
		   )
	)
  )
  (princ)
)


(defun opentxt (filepath / lst)
  (setq	lst	 nil
		file (open filepath "r")
  )
  (while (setq line (read-line file))
	(setq lst (cons line lst))
  )
  (close file)
  (reverse lst)
)

										;lst1 为k表 lst2为数据表
(defun c:hbtxt (/ lst1 lst2)
  (princ "\no__o:开始合并,断面成果txt文件")
  (setq lst1 (opentxt "d:\\020\\hb1.txt"))
  (setq lst2 (opentxt "d:\\020\\hb2.txt"))

  (if (> (length lsy1) (length lst2))
	(setq lstt lst1
		  lst1 lst2
		  lst2 lstt
	)
  )
  (setq lstdic nil)
  (foreach line	lst1
	(setq k (#sparser line " "))
	(setq k (list (strcat "K" (car k)) (cadr k)))
	(setq lstdic (cons k lstdic))
  )
  (setq n1 0)
  (repeat (/ (length lst2) 3)
	(setq k (nth (* 3 n1) lst2))
	(setq kold k)
	(setq k (vl-string-trim " " k))
	(setq k (substr k 1 5))
	(setq k (assoc k lstdic))
	(setq k (strcat (car k) " " (cadr k)))
	(setq lst2 (subst k kold lst2))
	(setq n1 (1+ n1))
  )
  (setq path "d:\\020\\hb合并后的断面成果.txt")
  (if (and (setq File Path) (setq F2 (open File "w")))
	(progn
	  (setq n2 -1)
	  (while (setq line (nth (setq n2 (1+ n2)) lst2))
		(if	(= n2 0)
		  (princ line F2)
		  (princ (strcat "\n" line) F2)
		)
	  )
	  (close F2)
	)
  )										;if-end
  (princ (strcat "\no__o:合并成功,文件路径为: " path))
  (princ)
)




(defun Layer_Freeze	(layer)
  (setq LayerSel (vla-get-Layers AcadDocument))
  (setq LayerObj (vla-add LayerSel layer))
  (setq msg (vla-get-Freeze LayerObj))
  (if (= msg :vlax-true)
	(setq msg :vlax-false)
	(setq msg :vlax-true)
  )
  (vla-put-Freeze LayerObj msg)
  (if (= msg :vlax-true)
	(princ (STRCAT "图层：" (vla-get-Name LayerObj) " 已冻结."))
	(princ (STRCAT "图层：" (vla-get-Name LayerObj) " 已解冻."))
  )

  (princ)
)