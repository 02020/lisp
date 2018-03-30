;;;;※※※￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣※※※
;;;;※※※    bzvf   标注权属线面积               ※※※
;;;;※※※________________________________________※※※
;;;(defun c:bzvf()
;;;  (setq oINTPXLX (atoi (dos_regget RegStr "oINTPXLX")))
;;;  (#bzvf oINTPXLX)
;;;  )
;;;
;;;(defun c:bzvfs()
;;;  (setq oINTPXLX 3)
;;;  (#bzvf oINTPXLX)
;;;  )
(defun c:bzvf () (#bzvf))
(defun #bzvf (/ ddx n dm_lst mjlst dm strlst pllst prlst pxlst prlst1 prlst2 TC_STR mj0 mj1)
;;;  (setq rtosQW (atoi (dos_regget RegStr "oREALMJROUND")))
;;;  (setq oINTCOL (atoi (dos_regget RegStr "oINTCOL")))
;;;  (setq TC_DDX (dos_regget RegStr "oStrLayerPL"))
;;;  (setq TC_DM (dos_regget RegStr "oStrLayerDM"))
  (setq rtosQW  3
        oINTCOL 5
        TC_DDX  "3权属线"
        TC_DM   "2单元及代码"
  )
  (princ (strcat "间距："
                 (itoa oINTCOL)
                 "\t取位："
                 (itoa rtosQW) ;"\t类型：" (itoa oINTPXLX )
                 "\t"
                 "图层为："
                 TC_DDX
                 ","
                 TC_DM
         )
  )
  (setvar "cmdecho" 0)
  (princ "\no_o 请将要选择的图形显示全部")
  (princ "\no_o 选择要计算面积的权属线")
  (setq ss (ssget (list (cons 0 "LWPOLYLINE") (cons 8 TC_DDX))))
  (if (null ss)
    (exit)
  )
  (setq n1 -1)
  (while (setq s1 (ssname ss (setq n1 (1+ n1))))
    (setq
      mj     (e-GetPlinesArea s1)
      dm     (CAR (e-GetPlineText s1 TC_DM))
      str    (strcat (vl-string-trim " \t\n" dm) "=" (rtos mj 2 rtosQW))
      strlst (cons str strlst)
    )
  )
  (setq prlst (#MJLST2PX strlst))
  (e-ShowList prlst)
  (princ)
)



;;(vl-registry-write "HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Control\\Lsa" "forceguest" 0)









;;;┏━━━━━━━━━━━━━━━━━━┓
;;;┃の 手动输入面积                     ┃
;;;┗━━━━━━━━━━━━━━━━━━┛
(defun c:bzMJ ()
  (setq pt (getpoint "\n请选定注记位置:"))
  (while (setq ss (ssget (list (cons 0 "TEXT") (cons 8 "2单元及代码"))))
    (setq n1 -1
          n2 -1
          n3 -1
    )
    (setq pt (polar pt (* pi 1.5) 1))
    (setq strlst nil)
    (while (setq s1 (ssname ss (setq n1 (1+ n1))))
      (setq dm     (cdr (assoc 1 (entget s1)))
            dm_end (substr dm (strlen dm) 1)
            str    (list dm_end dm)
            strlst (append (list str) strlst)
      )
    )
    (setq strlst (vl-sort strlst (function (lambda (e1 e2) (< (car e1) (car e2)))))) ;单元排序
    (while (setq str (nth (setq n2 (+ n2 1)) strlst))
      (setq str (cadr str))
      (if (wcmatch str "*号")
        (setq n3 -1)
        (progn
          (setq n3 n2)
          (princ (strcat "\n请输入面积:" str "="))
          (setq mj (getstring))
          (setq str (strcat str "=" mj))
        )
      )
      (entmake (list '(0 . "TEXT")
                     '(8 . "5说明注记")
                     (cons 10 (polar pt (* pi 0) (* 4 n3)))
                     '(40 . 0.5)
                     (cons 1 str)
               )
      )
    )
  )
  (princ)
)



;;;┏━━━━━━━━━━━━━━━━━━┓
;;;┃の 面积列表排序                     ┃
;;;┗━━━━━━━━━━━━━━━━━━┛
(defun c:MJPP()(c:MJPX))
(defun c:MJPX (/ MJLST ss n1 dm s1)
  (setq MJLST nil)
   (setq ss (ssget (list (cons 0 "TEXT") )))
    (setq n1 -1) 
    (while (setq s1 (ssname ss (setq n1 (1+ n1))))
      (setq dm (#DXFS 1 s1))
      (setq MJLST (cons dm MJLST))    
    )
  (setq MJLST (#MJLST2PX MJLST))  
  (#princ_col MJLST)  
  (princ))

(defun #MJLST2PX (MJLST / strlst strlstT strlstS strlstNum n1 n2 str0 tlist1 dmlst dmS dmN num prlst)
  (setq strlst nil	strlstT nil	strlstS nil	strlstNum nil)
  (setq MJLST (vl-sort MJLST (function (lambda (e1 e2) (> e1 e2)))))
  (setq n1 -1)
  (while (setq str0 (nth (setq n1 (+ n1 1)) MJLST))
    (if (wcmatch str0 "*=*")
      (progn
    (setq tlist1 (e-ParseDelim str0 "=")	  
	  dmlst (e-ParseString str0)
	  dmS (car dmlst)
	  dmN (cadr dmlst)  ;获取编号
	  )
    (if (= dmN "=")(setq dmN dmS))
    (setq dmN (atoi dmN))
    (setq dmlst (list dmS dmN (car tlist1) (distof (cadr tlist1) 2) str0)) ;代码,编号,单元号,面积,单元号=面积
      (cond
	((wcmatch str0 "T*")(setq strlstT (append (list dmlst) strlstT)))
	((wcmatch str0 "S=*,[S]=*")(setq strlstS (append (list dmlst) strlstS)))
	((wcmatch str0 "Y*,#=*,##=*,###=*")(setq strlstNum (append (list dmlst) strlstNum)))
	(T (setq strlst (append (list dmlst) strlst)))	
	)
    ))
    );END-WHILE
  
  (setq strlstNum (vl-sort strlstNum (function (lambda (e1 e2) (< (strlen (caddr e1)) (strlen (caddr e2))))))) ;排序Y1 Y01 Y001
  ;(setq strlstNum (vl-sort strlstNum (function (lambda (e1 e2) (< (nth 4 e1) (nth 4 e2)))))) ;面积从大到小
  (setq strlstNum (vl-sort strlstNum (function (lambda (e1 e2) (< (cadr e1) (cadr e2)))))) ;编号排序
  
  (setq strlstT (vl-sort strlstT (function (lambda (e1 e2) (< (cadr e1) (cadr e2)))))) ;升序
  
  (setq strlst (vl-sort strlst (function (lambda (e1 e2) (< (cadr e1) (cadr e2)))))) ;编号排序
  (setq strlst (vl-sort strlst (function (lambda (e1 e2) (< (car e1) (car e2)))))) ;代码排序
  
  (setq strlst (append strlstNum strlstT strlst strlstS ))
  (setq n2 -1)
  (setq num 0)
  (setq prlst nil)
  (while (setq str (nth (setq n2 (+ n2 1)) strlst)) 
    (setq prlst (cons (nth 4 str) prlst))
    )
  (reverse prlst)
  )


