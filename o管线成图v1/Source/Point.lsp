(setq blockSize 0.2)


;;绘制编号
(defun GetOrderId ()
  (princ "\no__o 生成节点序号")
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
  ;;先转序成正常的再进行剔除重复
  (setq lstPt    (reverse lstPt)
        nodeList (GetUniquePointFromList lstPt)
  )
  (DeleteBlockIdByColor 251)
  (ShowBlock_Oh nodeList "0.2")
)



;;输入点属性
(defun SetBlockNodeAttr (/)
  (princ "\no__o 将块属性复制给高程点")
  (setq sameset "1") ;全局变量
   (setq  layerName "080_A"
    blockName "0-Id"
	  color 250
	  )
  (setq newStr (getstring "\no__o: 请输入属性值:"))
  (WHILE (and (princ "\n选择要进行操作的属性块")
	      (setq ss (ssget  	(list '(0 . "insert") (cons 62 color) (cons 2 blockName) (cons 8 layerName))))
	 )
    (BlockAttrEdit ss newStr)
  )
)

 ;| 合并0h块 |;
(defun CombineBlockId (/ ssOh lstss lstD lst)
  (princ "\no__o 合并块")
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
      (princ "\no__o 同一点的块数量<>2, 现在执行疑问点判断")
      (alert "同一点的块数量<>2\n\n现在执行 疑问点 判断")
      (foreach x lstG
	(setq pt (cdr (assoc 10 (entget (car x)))))
	(if (/= (length x) 2)
	  (progn
	    (vla-ZoomCenter
	      (vlax-get-acad-object)
	      (vlax-3d-point pt)
	      12
	    )
	    (prompt "<ESC> 定位下一个点:")
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
      (setq op (getstring "\no__o 确认执行:[是(Y)]"))
      (if (or (= op "Y") (= op "y"))
	(command "_ERASE" ss "")
      )
    )
  )
)











 ;|
2.提取高程点(根据矢量箭头绘制Oh块)
执行后需要的输入节点性质
|;
(defun AddBlockH (/ lstpt ssD ssH ss2 lstN strBlock olayerStr)
  (princ "\no__o 提取高程点(根据矢量箭头绘制节点块)")
   (setvar "CLAYER" "080_A")
  (setq
    layerName "080_A"
    blockName "0-Id"
    blockSize 0.2
   )
  (setq ssPl (ssget "x" (list '(0 . "LWPOLYLINE") (cons 8 "080_A"))))
  (setq ssH (ssget "x" (list '(0 . "insert") (cons 2 "0-Elevation"))))
  (if (null ssH)
    (princ "\no__o 不存在 高程点 数据")
  )
  (if (null ssPl)
    (princ "\no__o 不存在 箭头 数据")
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
  (setq	prstr (strcat "\n实际 高程点个数为: "
		      (itoa (length (th-list-only lstPt)))
		      "\n目前 高程点个数为: "
		      (itoa (length lstN))
	      )
  )
  (princ prstr)
  (alert prstr)


  (princ)
)

















;;获得曲线的图元列表 (start end)
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