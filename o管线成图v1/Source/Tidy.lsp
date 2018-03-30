(defun __________3.文档整理__________ () ())
;;图面整理


;| 根据节点性质插入各个点的块 |;
(defun InsertPointBlock	(/ strPr)
  (princ "\no__o  根据节点性质插入块")
  (cond
    ((null currentLayer)
     (princ "\no__o 请设置当前图层")
     (alert "请设置当前图层")
     (exit)
    )
    ((not (tblsearch "layer" currentLayer))
     (princ (strcat "\no__o " currentLayer " 图层不存在"))
     (alert (strcat currentLayer " 图层不存在"))
     (exit)
    )
    (t (princ (strcat "\no__o 当前的工作图层为 " currentLayer " ")))
  )
  (setvar "CLAYER" currentLayer)
  (setq blockSize "0.5")
  (setq
    ssOh (ssget	"x"
		(list '(0 . "insert") (cons 2 "0-Id") (cons 8 "080_A"))
	 )
  )
  (if (null ssOh)
    (princ "\no__o 不存在 节点块")
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
		  (princ (strcat "\no__o " attr "块 不存在"))
		  (VL-CMDF "INSERT" attr pt blockSize blockSize "" )
		)
	  )
      )
    )
  )
)



 ;| 2.根据 矢量箭头 绘制 多段线 |;
(defun Arrows2Pline (/ ssPl tfD lstss PlCo _pt lstPl)
  (princ "\no__o 根据 矢量箭头 绘制 多段线")
  (princ "\no__o 请选择箭头")
  (setvar "osmode" 0)
  (setq ssPl (ssget (list (cons 0 "LWPOLYLINE") (cons 8 "080_A"))))
  (if (null ssPl)
    (princ "\no__o 图层 080_A 不存在 矢量箭头")
    (progn
      (princ
	(strcat	"\no__o 获取到的 矢量箭头 个数为:"
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
      ;;遍历PlCo表的元素
      (setq lstPl nil)
      (foreach en PlCo
	(if (member _pt en)
	  (progn
	    ;;判断 _pt 是不是 en  的起点
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
      ;;绘制多端线
      (#EntMake_pline
	(list (cons 10 (reverse lstPl))
	      (cons 40 0.2)
	      (cons 8 "080_0")
	)
      )
    )
    ;;end while _pt
  )
  (princ "\no__o 绘制的 多段线 在080_0图层")
  (princ)
)





 ;| 将Oh转成文本 |;
(defun c:Oh->Text (/ strPr)
  (princ "\no__o Oh块->文本 ")
  (setq
    ssOh (ssget	"x"
		(list '(0 . "insert") (cons 2 "0h") (cons 8 "080_A"))
	 )
  )
  (command "UNDO" "BE")
  (setvar "CLAYER" "080_0")
  (if (null ssOh)
    (princ "\no__o 不存在 高程点 数据")
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



 ;| 冻结图层 080_* |;
(defun c:layerFreeze_080 ()
  (princ "\no__o 请提取在 080_0 图层上的图元")
  (setvar "CLAYER" "0")
  (Layer_Freeze "080_0")
  (Layer_Freeze "080_k")
  (Layer_Freeze "080_A")
)