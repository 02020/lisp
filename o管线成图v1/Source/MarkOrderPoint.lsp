

;;绘制编号
(defun MarkOrderPoint4Arrows ()
 
  (DelAllBolck "0-Id" "080_A")
  (setq nodeList (MarkOrderPointArrows))
  (ShowBlock_Oh nodeList "0.2")
)

(defun MarkOrderPointArrows (/ en lstPtN ssPl lstss tf)
  (setvar "osmode" 0)
  (princ (strcat "\no__o 按一定顺序标示出各点"))
  (setq ssPl (ssget "x" (list (cons 0 "LWPOLYLINE") (cons 8 "080_A"))))
  (if (null ssPl)
    (princ "\no__o 不存在 对象")
    (progn
      (princ (strcat "\no__o 获取到图形个数:"
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
      ;; PlCo 表数据为 (图元 起点 终点)
      (Arrows2PlColst PlCo nil)
    )
  )
)



;;将数据展示在dwg上
(defun ShowBlock_Oh (lstPtN blockSize)
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)

  (setq n2 -1)
  (command "UNDO" "BE")
  (setvar "CLAYER" "080_A")
  (setq samePoint (GetSamePointFromList lstPtN 3))
  (if samePoint
    ;;错误绘制
    (progn
      (setq strError (strcat "\no__o 存在错误的节点个数：" (rtos (length samePoint) 2 0)))
      (princ strError)
      (alert strError)
      (foreach pt samePoint
	(AddCircle pt configPointCircle configLayerTemp)
      )
    )
    (progn
      (setq strqz (strcase (getstring "\no__o 请输入编号前缀:")))
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


;;根据图元的集合获取定位四至
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




 ;| 递归判断原则 PlCo 不为空则递归 |;
(defun Arrows2PlColst (PlCo lstPtN / plObj PlCoN en PlCoN lstN pln re)
  ;(GetZoomWindow PlCo)
  (setq strP "\no__o 选择该区域内的 起始箭头")
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

  ;;提取多段线对象为表 (元素 起点 终点)
  (setq	PlCo (vl-remove en PlCo)
	pln  (length PlCo)
	re   0
  )

  ;进度条
  (setq dcl_id (load_dialog "scroll-bar"))
  (new_dialog "scrolling" dcl_id)

  ;;用来控制循环
  (while (and PlCo (>= pln re))
    ;"执行 " (rtos re)
    (set_tile "txt" (strcat  " 正在处理第：" (rtos (- pln (length PlCo)))))
    (setq schedule (* 2.0 (/ re 2.0 pln)))
    (scroll-bar schedule)
    
    (setq re (1+ re))
    ;;遍历所有新点
    (foreach ptt lstPtN
      ;;遍历PlCo表的元素
      (foreach en PlCo
	(if (member ptt en)
	  (progn
	    (setq PlCo (vl-remove en PlCo))
	    ;;如果ptt不是多段线的起点则倒置多段线
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
  
  ;进度条
  (done_dialog)
  (unload_dialog dcl_id)
  
  (if (null PlCo)
    (progn
      (GetUniquePointFromList lstPtN)
    )
    (Arrows2PlColst PlCo lstPtN)
  )
)


;;提取唯一点，取位4
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
;;提取唯一点，取位4
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


;;提取相同的点
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











 ;| 递归判断原则 PlCo 不为空则递归 |;
(defun PlCo2PlColst (PlCo lstPtN / plObj PlCoN en PlCoN lstN pln re)
  ;;提取多段线对象为表 (元素 起点 终点)
  (setq	PlCoN  nil
	en     (car PlCo) ;提取第一个元素
	PlCo   (cdr PlCo) ;减去第一个元素
	PlCoN  (cons en PlCoN)
	lstPtN (append lstPtN (cdr en))
  )
  (setq	pln (length Plco)
	re  0
  )
  ;;用来控制循环
	  ;(while (and Plco (>= pln re))
	  ;	(setq re (1+ re))
  ;;遍历所有新点
  (foreach ptt lstPtN
    ;;遍历PlCo表的元素
    (foreach en	PlCo
      (if (member ptt en)
	(progn
	  (setq PlCo (vl-remove en PlCo))
	  ;;如果ptt不是多段线的起点则倒置多段线
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

