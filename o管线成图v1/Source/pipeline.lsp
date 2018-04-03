;; 最后修改时间 2018年4月10日


 ;| 工具箱命令:GGT
__ 0.设置数据库
__ 1.导入坐标点数据（mdb）

__ 3.检查点属性 ---- 点情况，多余点，重复点
__ 4.（出图）整理块方向
__ 5.

__ 备注:井盖点 lstpt 集合为二维点，高程点为三维，需要转换


__ 2.根据 矢量箭头 绘制 多段线

__ 4.根据 多段线,按一定顺序标示出各点
|;



(defun c:vfcd()
 ; (vla-removefrommenubar "BMF工具")
  (e-CreateMenu
    '("BMF工具" (
	
		 ("系统图层切换...". "XSXTTC")
		)
      )
    )
  (PRINC))


(defun c:d ()
(setq menu (vlex-MenuGroups))
  (princ menu)
  (princ)


  
;(FetchPointIntoDwg)
)


(defun SetDataMdb ()
  (setq dbfile (e-GetFiles "选择mdb" "mdb|*.mdb*"))
  (if (= dbfile "")
    (princ "\no__o 程序退出")
    (e-regset "dataMdb" dbfile)
  )
  (princ)
)



(defun c:2()
    (SetDataMdb)
  )


;| 1.导入坐标点数据 |;
(defun FetchPointIntoDwg (/ bmList layerNot layerName schedule block filePath)
  (setvar "osmode" 0)
  (setq blockSize "0.5")
  (setq dbfile (e-regget "dataMdb"))
  (Setq conn (vlax-create-object "ADODB.Connection")) ;引用ADO控件
  (setq connstring (strcat "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" dbfile)) ;设置数据库连接字符串
  (vlax-invoke-method conn "open" connstring "" "" -1) ;打开数据库连接

  (setq dcl_id (load_dialog "scroll-bar"))
  (new_dialog "scrolling" dcl_id)

  (setq	sql	  (BulidSelectSql configPoint "点表")
	data	  (ADO_DoSQL conn sql)
	n	  -1
	i	  0
	num	  (length data)
	layerNot  nil
	blocksNot nil
  )

 ;排除首行
  (while (setq item (nth (setq i (1+ i)) data))
    ;;进度条
    (setq schedule (* 2.0 (/ n 2.0 num)))
    (scroll-bar schedule)
    (setq x    (nth 0 item)
	  y    (nth 1 item)
	  h    (nth 5 item)
	  cat  (nth 2 item)
	  code (nth 3 item)
	  node (nth 4 item) ;节点性质
    )

    ;;插入块
    (if	(and (not (null x)) (not (null y)) (not (null h)))
	(InserPipelineBlock item)
    )
  )
  (setq layStr (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) layerNot)))
  (princ (strcat "\no__o 以下编码不存在对应图层 " layStr))
  (princ "\no__o 请检查核对节点所在的图层,手动进行调整 ")
  ;;卸载进度条
  (done_dialog)
  (unload_dialog dcl_id)
  (ADO_DisconnectFromDB conn)
  (princ)
)

;;插入管线块
(defun InserPipelineBlock (item)
  (setq	x	  (nth 0 item)
	y	  (nth 1 item)
	h	  (nth 5 item)
	cat	  (nth 2 item)
	code	  (nth 3 item)
	node	  (nth 4 item) ;节点性质
	pt	  (list (distof x 2) (distof y 2))
	layerName (GetLayerFormNode cat "POINT")
  )

  (if (/= (TBLSEARCH "BLOCK" node) nil)
    (progn
      (setq block (vla-insertblock mSpace (vlax-3d-point pt) node blockSize blockSize blockSize 0))
      (SetPipelineBlockXDatas block configPoint item)
      (if layerName
	(vla-put-layer block layerName)
	(if (not (member cat layerNot))
	  (setq layerNot (cons cat layerNot))
	)
      )
    )
    (progn
      (if (not (member node blocksNot))
	(setq blocksNot (cons node blocksNot))
      )
    )
  )
)

;;增加块的xData属性值
(defun SetPipelineBlockXDatas(o fields item)
  (defun XData (o key value)
    (ex:PutXData o (list (cons 1001 key) (cons 1000 value)))
  )
  (foreach field fields
   (setq value (nth (- (car field) 1) item ))
   (XData o (cadr field) value)
  )
)

;;获取xData属性值
(defun GetXDatas (en keyList dem)
  (setq	o	(vlax-ename->vla-object en)
	keyList	(mapcar '(lambda (x) (cadr x)) keyList)
  )
  (if (assoc -3 (entget en keyList))
    (apply
      'strcat
      (mapcar '(lambda (x) (strcat (GetXData o x) dem)) keyList)
    )
    nil
  )
)

;;获取xData属性值
(defun GetXDataMap (en keyList)
  (setq	o	(vlax-ename->vla-object en)
	keyList	(mapcar '(lambda (x) (cadr x)) keyList)
  )
  (if (assoc -3 (entget en keyList))

      (mapcar '(lambda (x) (list x (GetXData o x) )) keyList)
    
    nil
  )
)



;;
(defun GetXData	(vlaObj key)
  (cdr (assoc 1000 (ex:GetXData vlaObj key)))
)







;| 显示管径数据 |;
(defun c:s ()
  (setq fontSize "0.5")
  (setvar "pickbox" 20)
  (while (setq en (entsel "\no__o 请选选择 箭头"))
    (setq text (GetXDatas (car en) configPoint ","))
    (if	text
      (princ (strcat "\no__o " text))
      (princ "\no__o 该箭头未设置数据")
    )
  )
  (setvar "pickbox" 10)
)



;| 修改属性值mdb |;
(defun UpdatePointData ()
  (princ (strcat "\no__o " "修改属性值"))
  (setq dbfile "D:\\C3.CAD\\03.Code\\o管线成图v1\\Data\\成果1\\丽日花园点线库.mdb")
  (Setq conn (vlax-create-object "ADODB.Connection")) ;引用ADO控件
  (setq connstring (strcat "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" dbfile)) ;设置数据库连接字符串
  (vlax-invoke-method conn "open" connstring "" "" -1) ;打开数据库连接
  (setq tableName "点表")
  (setvar "pickbox" 20)
  (while (setq en (entsel "\no__o 请选选择 箭头"))
    (setq x	 (GetXDataMap (car en) configPoint)
	  x	 (dos_proplist "节点属性" "修改属性" x)
	  code   (cdr (assoc "外业点号" x))
	  strSql (BulidUpdateSql conn tableName x "外业点号")
    )
    (if (vl-symbol-value   	(ADO_DoSQL conn strSql))
	(princ (strcat "\no__o " code " 修改成功"))
    )
  )
  (setvar "pickbox" 10)
    (ADO_DisconnectFromDB conn)
  (princ)
)





(defun c:ss()
(UpdatePointData)
  )



;;;
;;;
;;;
;;;(setq x 
;;;
;;;(("Title" . "Floorplan") ("Project" . "Project A")) 
;;;
;;; 
;;;
;;;Command: 
;;;
;;;(("Title" . "Floorplan") ("Project" . "Project B")) 
;;;
;;; 
;;;
;;;



;;根据节点获取图层
(defun GetLayerFormNode	(bm lx / layer layerName layerColor)
 ;; (defun *error* (msg)(princ (strcat "\no__o GetLayerFormNode:" msg)))
  ;configLayerList configNodeList
  (setq 
    LayerSel (vla-get-Layers AcadDocument)
    layer    (assoc bm configLayerList)
  )
  (if (null layer)
    (progn nil)
    (progn
      (setq layerName  (strcat (cadr layer) lx)
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

