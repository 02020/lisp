;; 最后修改时间 2018年2月10日

;| 工具箱命令:GGT
__ 1.根据点绘制矢量箭头
__ 2.根据矢量箭头提取高程点数据,绘制 Oh 块
__ 3.补全高程点,输入高差
__ 4.根据 箭头,按一定顺序标示出各点
__ 5.合并同一点的Oh块

__ 备注:井盖点 lstpt 集合为二维点，高程点为三维，需要转换


__ 2.根据 矢量箭头 绘制 多段线

__ 4.根据 多段线,按一定顺序标示出各点
|;



 ;| 工具箱命令:GGT
__ 0.系统初始化
__ 1.导入坐标点数据
__ 2.设置当前操作的工作层
__ 3.输入点属性 ---- 点情况，多余点，重复点
__ 4.（出图）根据节点性质插入各个点的块
__ 5.

__ 备注:井盖点 lstpt 集合为二维点，高程点为三维，需要转换


__ 2.根据 矢量箭头 绘制 多段线

__ 4.根据 多段线,按一定顺序标示出各点
|;

;;块的三次递进 Elevation->h(高程)->h(id)->attr

(defun Init (/ currentLayer)
  ;; (#MakeLayers configLayerList) ;加载图层
  (CreateBlockId)
  (CreateBlockElevation)
  (CreateBlockAttr configPoint)
)


(defun c:d()
  (MakeArrows)
  )


 ;| 1.导入坐标点数据 |;
(defun FetchPointIntoDwg (/ bmList layerNot layerName schedule block filePath)
  (setvar "osmode" 0)
  (setq blockSize "0.5")
  (setq filePath (GetFiles))
 ;(setq filePath "D:\\C3.CAD\\03.Code\\o管线成图v1\\Data\\成果1\\丽日花苑原始坐标数据.txt")

	  ;(vl-cmdf "UNDO" "BE")
    (if (null filePath) (exit))
  (setq dcl_id (load_dialog "scroll-bar"))
  (new_dialog "scrolling" dcl_id)

  (setq lst (ReadTxt filePath)
	n 0
	num (length lst)
	)

  (setq layerNot nil)

  (foreach line	lst
    ;;进度条
    (setq schedule (* 2.0 (/ n 2.0 num)))
    (scroll-bar schedule)

    (setq k (#sparser line ",")
	  h (nth 4 k) ;高程
	  n (1+ n)
	  x (nth 3 k)
	  y (nth 2 k)
    )

    ;;插入块
    (if	(and (/= x "")(/= y "")  h)
      (progn
	(setq pt     (list (distof (cadddr k) 2) (distof (caddr k) 2))
	      bmList (StrDivEnInt (nth 1 k))
	)
	(setq block (InsertBlockAttr pt blockSize "0-Elevation" (list (nth 1 k) h)))

	(cond
	  ((= (length bmList) 3)
	   (setq dm (cadr bmList))
	  )
	  ((= (length bmList) 2)
	   (if (wcmatch (car bmList) "#")
	     (setq dm (cadr bmList))
	     (setq dm (car bmList))
	   )
	  )
	)
	(setq layerName (GetLayerFormNode dm))
	(if layerName
	  (vla-put-layer block layerName)
	  (if (not (member dm layerNot))
	    (setq layerNot (cons dm layerNot))
	  )
	)
      )
    )
  )
  (setq layStr (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) layerNot)))
  (princ (strcat "\no__o 以下编码不存在对应图层 " layStr))
  (princ "\no__o 请检查核对节点所在的图层,手动进行调整 ")
  ;;卸载进度条
  (done_dialog)
  (unload_dialog dcl_id)
(princ)
)

(defun GetLayerFormNode9 (bm)
  (setq color 1)
  (setq layerName (assoc bm configNodeSortList))
  (if (null layerName)
    nil
    (progn
      (setq layerName  (cadr layerName)
	    layerColor (assoc layerName configLayerList)
      )
      (if (not (null layerColor))
	(setq color (cadr layerColor))
      )
      (if (not (tblsearch "layer" layerName))
	(vla-put-Color (vla-add LayerSel layerName) color)
      )
      layerName
    )
  )
)

;;根据节点获取图层
(defun GetLayerFormNode	(bm / layer layerName layerColor)
  (defun *error* (msg)(princ (strcat "\no__o GetLayerFormNode:" msg)))
  ;configLayerList configNodeList
  (setq 
    LayerSel (vla-get-Layers AcadDocument)
    layer    (assoc bm configLayerList)
  )
  (if (null layer)
    (progn nil)
    (progn
      (setq layerName  (cadr layer)
	    layerColor (caddr layer)
      )
      (if (not (tblsearch "layer" layerName))
	(vla-put-Color (vla-add LayerSel layerName) layerColor)
      )
      layerName
    )
  )
)


;;删除重复数据
(defun EraseRepeatData (/ ss ss2 c ent att)
  (princ "\no__o 删除重复数据")
  (setq blockName "0-Elevation")
  (if (setq ss (ssget (list (cons 0 "INSERT") (cons 2 blockName))))
    (progn
      (setq c 0
	    ptlist nil
      )
      (repeat (sslength ss)
	(setq
	 current (ssname ss c)
	  ent (vlax-ename->vla-object current)
	      pt  (cdr (assoc 10 (entget current)))
	)
	(if  (member pt ptlist)
	  (progn
	    (vla-highlight ent :vlax-true)
	    (if	(not ss2)
	      (setq ss2 (ssadd current))
	      (ssadd current ss2)
	    )
	  )
	   (setq ptlist (cons pt ptlist))
	)
	(setq c (1+ c))
      )
    )
  )
  (if ss2
    (progn
    (command "erase" ss2 "")
    
    
  (princ (strcat "\no__o 节点个数：：" (itoa (sslength ss)) "，其中删除重复的个数为：" (itoa (sslength ss2))))
    )
    )
)


;|

	(if (not (null layerName))
	  (progn
	  (setq layerName (getstring (strcat "\no__o 请输入编码 " bm " 代表图层的名字 "))	    )
	    (vla-add LayerSel layerName)
	    (setq layerList (cons bm layerList)
	    )
	  )
	)

 |;



;; 1.设置当前图层
;; currentLayer 全局变量
(defun SetCurrentLayer ()
  (setq currentLayer (getstring "\no__o 请输入当前要操作的工作层："))
  (setvar "CLAYER" currentLayer)
)


 ;|

    (VL-CMDF "INSERT" "0attr" pt "1" "1" "" (nth 1 k) h )
    (repeat (- (length configPoint) 1)
      (vl-cmdf "")
    )
    (VL-CMDF "change" (entlast) "" "p" "la" layer "")


|;








 ;| 输出成果数据 |;
(defun ExportDataNodeArrow (/ ssPl ssoh lstssPl lstssOh lstOh lstPl)
  (defun Cad2txt_Arrow (lst Path /)
    (if	(and (setq File Path) (setq F2 (open File "w")))
      (progn
	(princ "序号|起点|终点|编号|长度|类型|管径|孔根数|埋设方式" F2)
	(setq n2 -1)
	(while (setq pt (nth (setq n2 (1+ n2)) lst))
	  (setq	pts  (nth 0 pt)
		pte  (nth 1 pt)
		data (nth 2 pt)
	  )
	  (princ
	    (strcat "\n"
		    (itoa (1+ n2))
		    "|"
		    (rtos (cadr pts))
		    ","
		    (rtos (car pts))
		    "|"
		    (rtos (cadr pte))
		    ","
		    (rtos (car pte))
		    "|"
		    data

	    )
	    F2
	  )
	)
	(close F2)
      )
      (princ "\no__o 无法输出")
    )
  )
  (defun Cad2txt_oh (lst Path /)
    (if	(and (setq File Path) (setq F2 (open File "w")))
      (progn
	(princ "序号|编号|高程|高差|坐标|节点性质" F2)
	(setq n2 -1)
	(while (setq lstoh (nth (setq n2 (1+ n2)) lst))
	  (setq pt (cdr (assoc 10 lstoh)))
	  (setq ptstr (strcat (rtos (cadr pt)) "," (rtos (car pt))))
	  (princ (strcat "\n"
			 (itoa (1+ n2))
		    	"|"
			 (cdr (assoc "id" lstoh))
			 "|"
			 (cdr (assoc "h" lstoh))
			 "|"
			 (cdr (assoc "hc" lstoh))
			 "|"
			 ptstr
			 "|"
			 (cdr (assoc "node" lstoh))
		 )
		 F2
	  )
	)
	(close F2)
      )
      (princ "\no__o 无法输出")
    )
  )

  ;;起点 终点 数据（编号，属性）
  (defun GetDiameterData (x)
    (setq en	  (cadr x)
	  obj	  (vlax-ename->vla-object en)
	  keyList (list "id" "length" "lx" "diameter" "hole" "inbuilt")
	  ;;剔除编号的属性值
	  xdata (assoc -3 (entget en (cdr keyList)) )
    )
    (list
      (vlax-curve-getStartPoint obj)
      (vlax-curve-getEndPoint obj)
      (if xdata
	(GetXData->diameter en keyList "|")
	(GetXData->diameter en (list "id") "|")
      )
    )
  )

  (setq ssPl (ssget "x" (list '(0 . "LWPOLYLINE") (cons 8 "080_A"))))
  (setq ssoh (ssget "x" (list '(0 . "insert") (cons 2 "0-Id"))))
  (if (null ssOh)
    (princ "\no__o 不存在 高程点 数据")
    (setq tfH t)
  )
  (if (null ssPl)
    (princ "\no__o 不存在 箭头 数据")
    (setq tfPl t)
  )
  (if (and tfH tfPl)
    (progn
      (princ (strcat "\no__o 获取到的 高程 个数为:"
		     (itoa (sslength ssoh))
	     )
      )

      (setq
	lstPl (mapcar 'GetDiameterData (ssnameX ssPl))
	lstOh (mapcar
		'(lambda (x)
		   (cons (assoc 10 (entget (cadr x)))
			 (BlockGetAtts (cadr x))
		   )
		 )
		(ssnameX ssOh)
	      )
      )

      (Cad2txt_oh lstOh (nth 0 configExportDataFilePath))
      (Cad2txt_Arrow lstPl (nth 1 configExportDataFilePath))
    )
  )
  (princ "\no__o 执行完毕")
  (princ)
  )

