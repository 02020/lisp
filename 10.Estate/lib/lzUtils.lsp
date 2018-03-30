;|______________________________________________________________________
	所有的工具函数
	用于其他工程
	加载时初始化代码
	全局变量
	常量

______________________________________________________________________|;

;;;--------------------------------------------------------------------;
;;;  First step is to load ActiveX functionality.  If ActiveX support  ;
;;;  already exists in document (can occur when Bonus tools have been  ;
;;;  loaded into AutoCAD), nothing happens.                            ;
;;;--------------------------------------------------------------------;

(progn
  (vl-load-com)
  (vl-load-reactors)
  (setvar "cmdecho" 0)
)
;;;--------------------------------------------------------------------;
;;;  For ActiveX functions, we need to define a global variable which  ;
;;;  "points" to the Model Space portion of the active drawing.  This  ;
;;;  variable, named *ModelSpace* will be created at load time.        ;
;;;--------------------------------------------------------------------;
(setq *ModelSpace*
		   (vla-get-ModelSpace
		     (vla-get-ActiveDocument (vlax-get-acad-object))
		   )
      AcadObject   (vlax-get-acad-object)
      AcadDocument (vla-get-ActiveDocument AcadObject)
      mSpace	   (vla-get-ModelSpace AcadDocument)

 ;_ end of vla-get-ModelSpace
) ;_ end of setq



(defun *error* (msg)
  (error "" msg)
)

(defun error (fnName msg)
  (if (wcmatch (strcase msg t) "*break,*cancel*,*exit*,*取消*")
    (progn (princ "\no__o 程序退出..."))
    (princ (strcat "\no__o " fnName ":" msg))
  )
)


;;;(defun *error* (msg)
;;;  (if (wcmatch (strcase msg t) "*break,*cancel*,*exit*,*取消*")
;;;    (princ)
;;;    (vla-EndUndoMark
;;;      (vla-get-ActiveDocument (vlax-get-acad-object))
;;;    )
;;;  )
;;;  (princ)
;;;)




;;;;※※※￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣※※※
;;;;※※※                输入的数值排列列数              ※※※
;;;;※※※                打印-表中值                     ※※※
;;;;※※※________________________________________________※※※

(defun e-ShowList(strlst / n4 n5 n6 text T40)
  (princ "\n按输入的数值排列列数" )
  (setq n4 -1 n5 1 n6 0)

  (setq col 5
   spjj 6)
  (while (null (setq pt (getpoint "\n请选定注记位置:"))))
  (setq ptt pt czjj 1)
  (setq T40 0.5)  
  (command "undo" "be")
  (while (setq text (nth (setq n4 (1+ n4)) strlst))
    (if (> n4 (- (* col n5) 1) ) (setq ptt (polar pt (* pi 1.5) (* czjj n5)) n5 (1+ n5) n6 0) )
    (entmake (list '(0 . "TEXT") '(8 . "5说明注记") (cons 10 (polar ptt 0 (* spjj n6))) '(40 . 0.5) (cons 1 text)))       
    (setq  n6 (1+ n6))
    )
  (command "undo" "e")
  (princ))



;;;;※※※￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣※※※
;;;;※※※                将字符串分割成表                ※※※
;;;;※※※________________________________________________※※※

(defun e-ParseDelim (str delim / ptr lst)
  (setq lst '())
  (setq len (+ (strlen delim) 1))
  (while (setq ptr (vl-string-search delim str))
    (setq lst (cons (substr str 1 ptr) lst ))
    (setq str (substr str (+ ptr len)))
    )
  (reverse (cons str lst))
  )


(defun e-ParseString (st / stl lst i ii key)
  (setq	lst '()	i 1 ii 1)
  (setq	stl (strlen st) )
  (repeat stl
    (if (or (wcmatch (substr st i 1) "#" )
	    (wcmatch (substr st i 2) "[+-.e]#,e[+-],[+-].")  
         )
        (setq key "nom")
        (setq key "str")
     );end if 
    (if (= 1 i) (setq key2 key))
    (cond
       ((= stl i)(setq lst (cons (substr st ii (- (1+ i) ii)) lst)) )
       ((/= key2 key)
        (setq lst (cons (substr st ii (- i ii)) lst)
              ii i
              key2 key
        ))
     )
    (setq i (1+ i))    
  )
(setq lst (reverse lst))
)

(defun e-ParseStringAll (st / stl lst i ii key)
  (setq	lst '()	i 1 ii 1)
  (setq	stl (strlen st) )
  (repeat stl
    (if (wcmatch (substr st i 1) "#" ) (setq key "nom") (setq key "str"))
    (if (= 1 i) (setq key2 key)) ;判定首次循环
    (if (/= key2 key)
      (setq lst (cons (substr st ii (- i ii)) lst)
	    ii i
	    key2 key
	    )
      )       
    (if (= stl i)(setq lst (cons (substr st ii (- (1+ i) ii)) lst)) )
    (setq i (1+ i))    
  )
(setq lst (reverse lst))
)

(defun e-ParseStringEvery (st / stl lst i ii key)
  (setq	lst '()	i 1 FT T)
  (while FT
    (if (wcmatch (substr st i 1) "#,@" ) (setq key 1) (setq key 2))    
    (setq lst (cons (substr st i key) lst))   
    (setq i (+ i key))
    (if (< (strlen st) i) (setq FT nil))
  )
(setq lst (reverse lst))
)




;;;--------------------------------------------------------------------;
;;;     Function: getPerp-Distance-and-Angle                           ;
;;;--------------------------------------------------------------------;
;;;  Description: This function returns a list with the distance and   ;
;;;               perpendicular angle to user pt3, and is determined   ;
;;;               by supplied points pt1 pt2.  Pt3 is "user input"     ;
;;;               and need not be at right angles.  This allows us to  ;
;;;               solve for cases where ortho mode is off.             ;
;;;  Example usage:                                                    ;
;;;        (setq Data  (getPerp-Distance-and-Angle pt1 pt2 pt3) )      ;
;;;--------------------------------------------------------------------;
;;;      Arguments:                                                    ;
;;;          pt1  seed point                                           ;
;;;          pt2  seed point                                           ;
;;;      Note:  pt1 and pt2 denote a "line" segment                    ;
;;;          pt3  "user point" (point to solve for)                    ;
;;;--------------------------------------------------------------------;

;|
__
__
__
|;

(defun lz-highlight (enCo)
  (mapcar '(lambda (x)
	     (vla-highlight (vlax-ename->vla-object x) :vlax-true)
	   )
	  enCo
  )
)

;;; 建立新的图层
	  ; (#MakeLayers (list '("0_659" 1) '("0_665" 2) '("0_669" 3)))
(defun #MakeLayers (lst)
  (setq	AcadObject   (vlax-get-acad-object)
	AcadDocument (vla-get-ActiveDocument AcadObject)
	mSpace	     (vla-get-ModelSpace AcadDocument)
  )
  (setq LayerSel (vla-get-Layers AcadDocument))
  (mapcar '(lambda (x)
	     (vla-put-Color (vla-add LayerSel (car x)) (cadr x))
	   )
	  lst
  )
)

	  ;字符串中含有List中的元素
(defun IsExistStringInList (sList string i)
  (apply 'or
	 (mapcar
	   '(lambda (x)
	      (equal (nth (1- i) x) string)
	    )
	   sList
	 )
  )
)

(defun ReadTxt (filepath / lst)
  (setq	lst  nil
	file (open filepath "r")
  )
  (while (setq line (read-line file))
    (setq lst (cons line lst))
  )
  (close file)
  (reverse lst)
)



;;根据图层及块名删除全部的块
(defun DelAllBolck (blockName layerName)
  (setq
    ss (ssget
	 "x"
	 (list '(0 . "insert") (cons 2 blockName) (cons 8 layerName))
       )
  )
  (if ss
    (command "_ERASE" ss "")
  )
)

;;;;※※※￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣※※※
;;;;※※※                将字符串分割成表                ※※※
;;;;※※※________________________________________________※※※

(defun StrDivEnInt (st / stl lst i ii key)
  (setq	lst '()
	i   1
	ii  1
	stl (strlen st)
  )
  (repeat stl
    (if	(or (wcmatch (substr st i 1) "#")
	)
      (setq key "integer")
      (setq key "string")
    )
    (if	(= 1 i)
      (setq key2 key)
    )
    (if	(/= key2 key)
      (setq lst	 (cons (substr st ii (- i ii)) lst)
	    ii	 i
	    key2 key
      )
    )
    (if	(= stl i)
      (setq lst (cons (substr st ii (- (1+ i) ii)) lst))
    )
    (setq i (1+ i))
  )
  (setq lst (reverse lst))
)






	  ;文件选择对话
(defun GetFiles	(/)
  (if (/= (vl-registry-read "HKEY_CLASSES_ROOT\\Licenses\\4D553650-6ABE-11cf-8ADB-00AA00C00905")
	  "gfjmrfkfifkmkfffrlmmgmhmnlulkmfmqkqj"
      )
    (vl-registry-write "HKEY_CLASSES_ROOT\\Licenses\\4D553650-6ABE-11cf-8ADB-00AA00C00905"
		       ""
		       "gfjmrfkfifkmkfffrlmmgmhmnlulkmfmqkqj"
    )
  )
  (if (setq x (Vlax-Get-Or-Create-Object "MSComDlg.CommonDialog"))
    (progn (vlax-put-property x "DialogTitle" "请选择坐标数据")
	   (vlax-put-property x "Filter" "DWG Files|*.txt|All Files|*.*")
	   (vlax-put-property x "MaxFileSize" 10000)
	  ;        (vlax-put-property x "Flags" 512)
	   (vlax-put-property x "Flags" 1574404)
	   (vlax-put-property x "Action" 1)
	   (vlax-get x "Filename")
    )
    (princ "\no__o: GetFiles 获取文件路径执行失败！")
  )
)




;;格式化点    
(defun FormatPoint (point num)
  (mapcar
    '(lambda (x)
       (fixqw x num) ;点取位
     )
    point
  )
)


;;增加点圆，增强点位置的显示
(defun AddCircle (pt pointCircle layer)
;;;  (setq layerSel (vla-get-Layers AcadDocument))
;;;  (setq layerObj (vla-add layerSel "080_0"))
  (setq	obj (vla-addCircle
	      (vlex-ModelSpace)
	      (vlax-3D-Point pt)
	      pointCircle
	    )
  )
  (vla-Put-Layer obj layer)
  (vla-Update obj)
  (vlax-Release-Object obj)
)

(defun scroll-bar (perc / icon_x icon_y)
  (setq	icon_x (dimx_tile "perc")
	icon_y (dimy_tile "perc")
  )					;setq
  (start_image "perc")
  (fill_image 0 0 (fix (* icon_x perc)) icon_y 1)
  (end_image)
)
 ;;选择集转化为表
(defun ss->enlist (ss / lst n en)
  (setq n -1)
  (while (setq en (ssname ss (setq n (1+ n))))
    (setq lst (cons en lst))
  )
)


;;;;※※※￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣※※※
;;;;※※※  #sparser  依据输入的字符将字符串分割组成表    ※※※
;;;;※※※       str  为需要处理的字符串                  ※※※
;;;;※※※     delim  字符串中需分割字符(除中文外)        ※※※
;;;;※※※------------------------------------------------※※※
;;;;※※※  #sparser2                                     ※※※
;;;;※※※     delim  字符串中需分割字符(中文字符)        ※※※
;;;;※※※________________________________________________※※※
(defun #sparser	(str delim / ptr lst)
  (setq lst '())
  (setq len (+ (strlen delim) 1))
  (while (setq ptr (vl-string-search delim str))
    (setq lst (cons (substr str 1 ptr) lst))
    (setq str (substr str (+ ptr len)))
  )
  (reverse (cons str lst))
)
 ;|
__ 提取多段线数据
__ Param ssget
__ Return (list en 起点 终点)
__ 目前应用:提取矢量箭头数据
|;
(defun ssPl->lst (ss / n1 lstss x plObj)
  (setq	n1 -1
	lstss nil
  )
  (while (setq ssna (ssname ssPl (setq n1 (1+ n1))))
    (setq lstss (cons ssna lstss))
  )
  (mapcar '(lambda (x)
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

;;;	(mapcar '(lambda (x)(setq PlCoAll
;;;	(vl-remove x PlCoAll))) PlCo)

(defun InsertEn	(pos lstI lstD / lstN)
  (setq lstN nil)
  (foreach n lstD
    (setq lstN (cons n lstN))
    (if	(= (vl-position n lstD) pos)
      (setq lstN (cons lstI lstN))
    )
  )
  (reverse lstN)
)


(defun InsertPt	(pos lstI lstD / lstN)
  (setq lstN nil)
  (foreach n lstD
    (setq lstN (append (list n) lstN))
    (if	(= (vl-position n lstD) pos)
      (setq lstN (append (reverse lstI) lstN))
    )
  )
  (reverse lstN)
)




;;从lst1中删移除lst2
(defun th-list-inter (lst1 lst2 / lst tmp)
  (setq lst '())
  (foreach tmp lst1
    (if	(not (member tmp lst2))
      (setq lst (cons tmp lst))
    )
  )
  (reverse lst)
)


(defun th-list-only (lst / lstN)
  (mapcar '(lambda (x)
	     (if (not (member x lstN))
	       (setq lstN (cons x lstN))
	     )
	   )
	  lst
  )
  lstN
)
	  ;获取多段线所有顶点坐标
(defun th2-get-allvertexs (e / n lst)
  (repeat (setq n (fix (1+ (vlax-curve-getendparam e))))
    (setq lst (cons (vlax-curve-getpointatparam e (setq n (1- n))) lst))
  )
)


	  ;提取重复的元素
(defun same (l1 / l2)
  (while l1
    (if	(member (car l1) (cdr l1))
      (setq l2 (append l2 (list (car l1))))
    )
    (setq l1 (vl-remove (car l1) l1))
  )
  l2
)




(defun lstRe (ptt lstD / lst1 lst2)
  (if (= (vl-position ptt lstD) 0)
    (reverse (vl-remove ptt lstD))
    (progn
      (foreach n lstD
	(if (< (vl-position n lstD) (vl-position ptt lstD))
	  (setq lst1 (append (list n) lst1))
	  (setq lst2 (append (list n) lst2))
	)
      )
      (append (reverse lst1) (reverse lst2))
    )
  )
)






(defun #copy_ss0 (ss layer px) ;F:最前 B:最后
  (if (null ss)
    (princ "无对象")
    (command "copy" ss "" "@" "@" "change" ss "" "p" "la" layer "" "_.Draworder" ss "" px)
  )
  (princ "执行完毕")
)



;;;┏━━━━━━━━━━━━━━━━━━┓
;;;┃の #Entmake_Text                    ┃
;;;┗━━━━━━━━━━━━━━━━━━┛
;;;(defun #Entmake_Text (Text pt T40)
;;;  (if (or (= (type text) 'INT) (= (type text) 'REAL))
;;;	(setq text (rtos text 2 2))
;;;  )
;;;  (if (null Text)
;;;	(setq Text "nil")
;;;  )
;;;  (entmake (list '(0 . "TEXT")
;;;				 (cons 10 pt)
;;;				 (cons 40 T40)
;;;				 (cons 1 Text)
;;;		   )
;;;  )
;;;)


(defun #Entmake_Text (Text pt T40 T50)
  (if (or (= (type text) 'INT) (= (type text) 'REAL))
    (setq text (rtos text 2 2))
  )
  (if (null Text)
    (setq Text "nil")
  )
  (entmake (list '(0 . "TEXT")
		 (cons 10 pt)
		 (cons 40 T40)
		 (cons 1 Text)
		 (cons 50 T50)
	   )
  )
)






;;;┏━━━━━━━━━━━━━━━━━━┓
;;;┃の #Entmake_pline 多段线            ┃
;;;┗━━━━━━━━━━━━━━━━━━┛
(defun #EntMake_pline (lst / k lstPt)
  (setq k (cdr (assoc 40 lst)))
  (setq lstPt (cdr (assoc 10 lst)))
  (entmakeX
    (append
      (list '(0 . "LWPOLYLINE")
	    '(100 . "AcDbEntity")
	    '(100 . "AcDbPolyline")
	    (cons 90 (length (cdr (assoc 10 lst))))
	    (cons 8 (cdr (assoc 8 lst)))
      )
      (apply 'append
	     (mapcar '(lambda (x)
			(list (cons 10 x) (cons 40 k) (cons 41 k) '(42 . 0.0))
		      )
		     lstPt
	     )
      )
    )
  )
)


(defun #EntMake_pline_k	(lst / PT)
  (entmakeX
    (append (list '(0 . "LWPOLYLINE")
		  '(100 . "AcDbEntity")
		  '(100 . "AcDbPolyline")
		  (cons 90 (length (car lst)))
		  (cons 8 (cadr lst))
	    )
	    (apply 'append
		   (mapcar '(lambda (x)
			      (list (cons 10 (car x))
				    (if	(null (cadr x))
				      (cons 40 0)
				      (cons 40 (cadr x))
				    )
				    (if	(null (cadr x))
				      (cons 41 0)
				      (progn
					(if (null (caddr x))
					  (cons 41 (cadr x))
					  (cons 41 (caddr x))
					)
				      )
				    )
				    '(42 . 0.0)
			      )
			    )
			   (car lst)
		   )
	    )
    )
  )
)






 ;|______________________________________________________________________

  块操作相关
1 获得块属性
2 设置块属性
3 判断 对象 是否存在
________________________________________________________________________
								      |;

 ;| 获得块属性 |;
(defun BlockGetAtts (ent / lst blkref a)
  (vl-load-com)
  (if (= (vla-Get-ObjectName
	   (setq blkref (vlax-Ename->vla-Object ent))
	 )
	 "AcDbBlockReference"
      )
    (if	(vla-Get-HasAttributes blkref)
      (progn
	(setq lst (vlax-safearray->list
		    (vlax-variant-value (vla-GetAttributes blkref))
		  )
	)
	(setq lst (mapcar 'cons
			  (mapcar 'vla-Get-TagString lst)
			  (mapcar 'vla-get-TextString lst)
		  )
	)

      )
    )	  ; endif
  )	  ; endif
  lst
)


 ;| 设置块属性 |;
(defun BlockSetAtts (Obj lst / AttVal)
  (mapcar
    '(lambda (Att)
       (if (member (vla-get-TagString Att) lst)
	 (vla-put-TextString Att (cadr lst))
       )
     )
    (vlax-invoke Obj "GetAttributes")
  )
  (vla-update Obj)
  (princ)
)




 ;| 判断 对象 是否存在
1 cltns: (vla-get-layers (vla-get-ActiveDocument (vlax-get-acad-object))))
2 cltns: (vla-get-blocks (vla-get-ActiveDocument (vlax-get-acad-object))))
3 objName 对象名
|;
(defun ObjSel (cltns objName / temp)
  (VLAX-FOR Obj	cltns
    (if	(vlax-property-available-p Obj 'Name)
      (if (= (vla-get-Name Obj) objName)
	(setq temp Obj)
      )
    )
  )
  (eval temp)
)


(defun BlokcIsExist (strName /)
  (setq BlockSel (vla-get-blocks acaddocument))
  (ObjSel blockSel strName)
)





 ;| 取位 |;
(defun fixqw (num power)
  (setq power (expt 10 (- power 0)))
  (/ (float (fix (* num power))) power)
)




 ;| 返回选择集中图元的点列表 为唯一值 |;
 ;|
__	( ( '(点1) '(点2) )	( '(点1 en1) '(点1 en2) ) )
|;
(defun lstss->lstPt (lstss / _pt lstPt lstPtx)
  (setq	lstPtx (mapcar '(lambda	(x)
			  (setq _pt (cdr (assoc 10 (entget x))))
			  (list (list (car _pt) (cadr _pt) 0) x)
			)
		       lstss
	       )
  )
  (mapcar '(lambda (x)
	     (setq x (car x))
	     (if (not (member x lstPt))
	       (setq lstPt (cons x lstPt))
	     )
	   )
	  lstPtx
  )
  (list lstPt lstPtx)
)

 ;| 将en集合根据点分组(递归) |;
(defun lstenPt->Group (lsten lstRec / en pt lstD lstRe)
  (setq	en    (car lsten)
	lsten (cdr lsten)
	pt    (cdr (assoc 10 (entget en)))
	pt    (list (car pt) (cadr pt) 0)
	lstD  nil
	lstD  (cons en lstD)

  )
  (foreach x lsten
    (setq _pt (cdr (assoc 10 (entget x))))
    (setq _pt (list (car _pt) (cadr _pt) 0))
    (if	(equal _pt pt 0.0001)
      (progn
	(setq lsten (vl-remove x lsten))
	(setq lstD (cons x lstD))
      )
    )
  )
  (setq lstRec (cons lstD lstRec))

  (if (null lsten)
    lstRec
    (lstenPt->Group lsten lstRec)
  )
)




 ;|
rtos 将数字转换成字符串 (rtos number [mode [precision]])
itoa 将整数转换成字符串，并返回转换结果
atof 将一个字符串转换成实数 (atof string)
atoi 将一个字符串转换成整数 (atoi string)
distof 将一个表示实（浮点）数的字符串转换成一个实数
rem 将第一个数除以第二个数，并返回余数
minusp 检查某个数是否是负数
|;



 ;| 将输入的数据转换为字符串列表  |;
(defun str2lst (str /)
  (read	(vl-list->string
	  (apply 'append
		 (mapcar
		   '(lambda (x)
		      (if (= 32 x)
			(list 34 32 34)
			(list x)
		      )
		    )
		   (append (list 40 34) (vl-string->list str) (list 34 41))
		 )
	  )
	)
  )
)












