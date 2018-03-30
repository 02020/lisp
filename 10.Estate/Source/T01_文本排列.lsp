(setvar "cmdecho" 0)
(defun c:tk()(c:tcircle))
(defun c:tdqd()(acet-tjust (ssget) "Start"))
;;;┏━━━━━━━━━━━━━━━━━━┓
;;;┃の TG 水平垂直对齐                  ┃
;;;┗━━━━━━━━━━━━━━━━━━┛
(defun c:TQh()(#Tdq 1))
(defun c:TQv()(#Tdq 2))
(defun c:TGh()(#Tdq 1))
(defun c:TGv()(#Tdq 2))

(defun #Tdq(lx / jj ent t1 pt pt_x pt_y )
  (c:66)
  (princ "\n水平或垂直对齐文本")
  (princ "\n选择对齐文本")
  (setq ss (ssget ":S" (list (cons 0 "TEXT") )))
  (if (not ss)
    (progn
      (alert "重新选择")
      (setq ss (ssget ":S" (list (cons 0 "MTEXT") )))
      )
    (if (not ss)(exit))
      )

  
  (setq ptA (cdr (assoc 10 (entget (ssname ss 0)))))
  (setq ptA_x (car ptA) ptA_y (cadr ptA))

  (setq ptB (cdr (assoc 11 (entget (ssname ss 0)))))
  (setq ptB_x (car ptB) ptB_y (cadr ptB))  
  
  (alert "\n选择文本")
  (while (setq ss (ssget))
    (princ "\n选择文本")
    ;(setq flag "Start")
    ;(acet-tjust ss flag)  ;引用express工具
    (setq n1 -1)
      (while (setq s1 (ssname ss (setq n1 (1+ n1))))
	(setq pt0 (cdr (assoc 10 (entget s1))))
	(setq pt1 (cdr (assoc 11 (entget s1))))
	(setq pt0_x (car pt0) pt0_y (cadr pt0))
	(setq pt1_x (car pt1) pt1_y (cadr pt1))
      (cond
	((= lx 1)(setq pt0 (list pt0_x ptA_y) pt1 (list pt1_x ptB_y)))
	((= lx 2)(setq pt0 (list ptA_x pt0_y) pt1 (list ptB_x pt1_y)))
	)
	(#XD-modent (entget s1) (list (cons 10 pt0) (cons 11 pt1)))
	)
    )
  (princ))


;;;┏━━━━━━━━━━━━━━━━━━┓
;;;┃の TGDW 整行文字对齐                ┃
;;;┗━━━━━━━━━━━━━━━━━━┛
(defun c:TQDW()(#tmove))
(defun #tmove()
  (princ "连续选择")
  (while (not (setq ent (car (entsel "\n 选择要对齐位置的文本")))))
  (setq pt (cdr (assoc 10 (entget ent))))
  (setq pt0_x (car pt) pt0_y (cadr pt))  
  (setq hjj 4 vjj 1)  
  (setq n1 1)
  (while (setq ss (ssget (list (cons 0 "TEXT") )))
    (setq n2 -1)
    (setq ss (#SORT-SE ss 10 0 0.1 nil)) ;X坐标值进行排序,顺序为从小到大
    (while (setq s1 (ssname ss (setq n2 (1+ n2))))
      (setq pt (cdr (assoc 10 (entget s1))))
      (setq pt_x (car pt) pt_y (cadr pt))
      (setq pt_x (+ pt0_x (* n2 hjj))
	    pt_y (- pt0_y (* n1 vjj))
	    )
      (sub_upd s1 10 (list pt_x pt_y))
      )
    (setq n1 (1+ n1))
    )
  (princ))


(defun c:TQ() 
  (setvar "osmode" 0)
  (setvar "CMDECHO" 0)
  (setq oINTCOL (distof (dos_regget RegStr "oINTCOL")))
  ;(setq oINTCOL 3)
  (princ "按行选择文字")
  (setq ptN0 (getpoint "\n 获取点：\n"))  
  (vl-cmdf "undo" "be")
  (setq n1 -1)
  (while (setq ss (ssget (list (cons 0 "TEXT") )))
    (setq flag "Start")
    (acet-tjust ss flag)  ;引用express工具
    (setq ptN0 (polar ptN0 (* pi 1.5) 1))
    (setq ss (#SORT-SE ss 10 0 0.1 nil)) ;X坐标值进行排序,顺序为从小到大
    (setq n2 -1)
    (setq n3 0)
    (setq n4 0)
    (setq strT (#DXFS 1 (ssname ss 0)))
    (if (= (substr strT (- (strlen strT) 1) ) "号")
      (progn
	(setq n3 -1)
	(setq n4 -2)
        ;(setq ptN0 (polar ptN0 (* pi 1.5) 1))
	)
      )
    (while (setq s1 (ssname ss (setq n2 (1+ n2))))
      (setq ptt (polar ptN0 0 (* (+ n2 n3) (+ oINTCOL n4))))     
      (#XD-modent (entget s1) (list (cons 10 ptt) ))
      ;(#XD-modent (entget s1) (list (cons 10 ptt) (cons 11 ptt)))
      (setq n4 0)
      )
    )
  (princ))

;;;┏━━━━━━━━━━━━━━━━━━┓
;;;┃の 字符间距调整                     ┃
;;;┗━━━━━━━━━━━━━━━━━━┛

(defun c:TQT () (#ttjj "ALL"))
(defun c:TQT0 () (#ttjj 0))
(defun c:TQT1 () (#ttjj 1)) ;换行向左
(defun c:TQT2 () (#ttjj 2)) ;换行向右
(defun c:TQT3 () (#ttjj 3)) ;换行向上
(defun c:TQT4 () (#ttjj 4)) ;换行向下


(defun #ttjj (lx / ent ss n1 jj )
  ;(setq oINTCOL (distof (dos_regget RegStr "oINTCOL")))
  (setq oINTCOL 5)
  (princ (strcat "字符间距调: " (rtos oINTCOL 2 1)))
  (cond 
    ((= lx "ALL")
     (progn
       (setq pt (getpoint "\n 获取点：\n"))  
       ;(setq pt (polar pt (* pi 0) oINTCOL))
       (setq n1 0)
       )
     )
    (T
     (progn
       (while (not (setq ent (entsel "\n 选择要对齐位置的文本"))))
       (setq pt (#DXFE 10 ent))
       (setq n1 -1)
       )
     )
    )  
  
  (setq ss (ssget (list (cons 0 "TEXT") )))
  (setq ss (#SORT-SE ss 10 0 0.1 nil)) ;X坐标值进行排序,顺序为从小到大
  
  (cond ;((ssmemb ent ss) (setq pt pt))
    ((= lx "ALL")  (setq pt (#DXFS 10 (ssname ss n1))))
    ((= lx 0) (setq pt (polar pt (* pi 0.0) oINTCOL)))
    ((= lx 1) (setq pt (polar pt (* pi 1.0) oINTCOL)))
    ((= lx 2) (setq pt (polar pt (* pi 0.0) oINTCOL)))
    ((= lx 3) (setq pt (polar pt (* pi 0.5) 1)))
    ((= lx 4) (setq pt (polar pt (* pi 1.5) 1)))
    
    )
  (while (setq s1 (ssname ss (setq n1 (1+ n1))))
    (setq ptt (polar pt 0 (*  n1 oINTCOL)))
    (sub_upd s1 10 ptt)
    )
  (princ))

;;;┏━━━━━━━━━━━━━━━━━━┓
;;;┃の TQDH 两文本互换位置              ┃
;;;┗━━━━━━━━━━━━━━━━━━┛
(defun c:TQdh()
  (princ "将选择的两个文本对换位置：")
  (setq ss (ssget (list (cons 0 "TEXT"))))
  (setq pt1 (#DXFS 10 (ssname ss 0)))
  (setq pt2 (#DXFS 10 (ssname ss 1)))
  (sub_upd (ssname ss 0) 10 pt2)
  (sub_upd (ssname ss 1) 10 pt1)
  (princ))

(defun c:TQdq()
  (princ "对齐单元：")
  (setq ss (ssget (list (cons 0 "TEXT"))))
  (setq lay (#DXFS 8 (ssname ss 0)))
  (setq pt0 (#DXFS 10 (ssname ss 0)))
  (setq pt1 (#DXFS 10 (ssname ss 1)))
  (if (= lay "2单元代码")
    (sub_upd (ssname ss 1) 10 pt0)
    (sub_upd (ssname ss 0) 10 pt1)
    )
  (princ))


;;;┏━━━━━━━━━━━━━━━━━━┓
;;;┃の TQX 文本点选手动排序             ┃
;;;┗━━━━━━━━━━━━━━━━━━┛

;;;;※※※￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣※※※
;;;;※※※  TQX  仅互换文字内容,不改变位置               ※※※
;;;;※※※  TQX1 按默认格式排版文字(并非文字原来位置)    ※※※
;;;;※※※________________________________________________※※※
(defun c:TQX () (#TQPX 0))
(defun c:TQX1() (#TQPX 1))
(defun c:TQX2() (#TQPX 2))

(defun #TQPX (lx / ptlst tlst text n1 pt)
  (command "undo" "be")
  (cond ((= lx 0) (setq text "\n 仅互换文字内容,不改变位置："))
	((= lx 1)
	 (setq text "\n 按默认格式排版文字(并非文字原来位置)：")
	 )
	((= lx 2)
	 (setq text "\n 按默认格式排版文字(并非文字原来位置)：")
	 (setq oINTCOL (ureal 1 "" "\n输入间距 " oINTCOL))
	 )
	
	)
  (princ text)
  (princ "\n\n 按顺序选择你要排序的文字：")
  (setvar "pickbox" 10)
  (while (setq ss (ssget ":s" (list (cons 0 "TEXT") )))
    (setq s1 (ssname ss 0))
    (setq pt  (#DXFS 10 s1)
	  txt (#DXFS 1 s1)
	  )
    (setq ptlst (cons pt ptlst) tlst (cons txt tlst))
    (entdel s1) 
    )
  (setq n1 -1)
  (setq ptlst (vl-sort ptlst (function (lambda (e1 e2) (< (car e1) (car e2))))))
  (setq pt (nth 0 ptlst) n1 0)
  (setq tlst (reverse tlst))
  (while (setq txt (nth n1 tlst))
    (cond ((= lx 0) (setq ptt (nth n1 ptlst) ))
	  ((= lx 1) (setq ptt (polar pt 0 (* oINTCOL n1)) ))
	  ((= lx 2) (setq ptt (polar pt 0 (* oINTCOL n1)) ))
	  )	  
    (entmake (list '(0 . "TEXT")  (cons 10 ptt) '(40 . 0.5) (cons 1 txt)))
    (setq n1 (+ n1 1))
    )
  (setvar "pickbox" 6)
  (command "undo" "e")
  (princ))


;;;┏━━━━━━━━━━━━━━━━━━┓
;;;┃の 根据每列文本个数排列             ┃
;;;┗━━━━━━━━━━━━━━━━━━┛
(defun c:TQC (/ ptlst tlst text n1 pt)
  (setq ss (ssget (list (cons 0 "TEXT") )))
  (setq ss (#SORT-SE ss 10 0 0.1 nil))
  (setq n1 -1) 
  (while (setq s1 (ssname ss (setq n1 (1+ n1))))
    (setq txt (#DXFS 1 s1))
    (setq tlst (cons txt tlst))
    )
  (setq tlst (reverse tlst))
  (#princ_col tlst)
  (princ))


(defun c:bdvf1 ()(c:TQC1))

(defun c:TQC1 (/ ptlst tlst text n1 pt)
  (setq ss (ssget (list (cons 0 "TEXT") )))
  (setq ss (#SORT-SE ss 10 0 0.1 nil))
  (setq n1 -1) 
  (while (setq s1 (ssname ss (setq n1 (1+ n1))))
    (setq txt (#DXFS 1 s1))
    (setq tlst (cons txt tlst))
    )
  ;(setq tlst (reverse tlst))
  (setq tlst (vl-sort tlst (function (lambda (e1 e2) (> e1 e2))))) 
  (#princ_pt tlst 4)
  (princ))

(defun c:TQXZD ()  
  (while (setq ss (ssget (list (cons 0 "TEXT") )))
    (setq strlst nil
	  strlst梯 nil
	  strlstS nil)
    (setq n1 -1)
    (while (setq s1 (ssname ss (setq n1 (1+ n1))))
      (setq str0 (#DXFS 1 s1))
      (setq tlist1 (#sparser str0 "=")
	    dm (car tlist1)
	    dm_end (atoi (substr dm (strlen dm) 1))) ;获取单元的类型
      (setq str (list dm_end (car tlist1) (atoi (cadr tlist1)) str0))
      ;(list 1 单元编号 面积 内容）)
      (cond
	((wcmatch str0 "T*")(setq strlst梯 (append (list str) strlst梯)))
	((wcmatch str0 "*S*")(setq strlstS (append (list str) strlstS)))
	(T (setq strlst (append (list str) strlst)))
	)
    )
    (setq strlst (vl-sort strlst (function (lambda (e1 e2) (> (caddr e1) (caddr e2)))))) ;单元排序
    (setq strlst (vl-sort strlst (function (lambda (e1 e2) (< (car e1) (car e2)))))) ;单元排序
    (setq strlst梯 (vl-sort strlst梯 (function (lambda (e1 e2) (< (car e1) (car e2)))))) ;单元排序
    (setq strlst (append strlst strlst梯 strlstS))
;;;    (setq n2 -1)
;;;    (while (setq str (nth (setq n2 (+ n2 1)) strlst))
;;;      (setq str (cadddr str))   
;;;     (entmake (list '(0 . "TEXT") '(8 . "5说明注记") (cons 10 (polar pt (* pi 0) (* 4 n2))) '(40 . 0.5) (cons 1 str)))
;;;      )
;;;    )
    (setq n2 -1)
    (setq prlst nil)
    (while (setq str (nth (setq n2 (+ n2 1)) strlst))
      (setq prlst (cons (cadddr str) prlst))
      )
    (setq prlst (reverse prlst))
    (#princ_col prlst)
    )
  (princ))

;拼合 字符打断,排列,打印位置
(defun c:TVF ()
  (princ "\n修改原面积计算<拼合 字符打断,排列,打印位置>")
  (setq strAll nil)
  (setq ss (ssget (list (cons 0 "TEXT"))))
  (setq n1 -1)
  (while (setq s1 (ssname ss (setq n1 (1+ n1))))
    (setq strAll (append (#tddtext s1) strAll ))
    )
  (princ strall)
  (setq prlst (#MJLST2PX strAll))
  (#princ_col prlst)
  )

(defun #tddtext (ent / n1 str ptt tlist RP n2 strlst)
  (setq n1 -1 )
  (setq str (#DXFS 1 ent))
  (setq ptt (#DXFS 10 ent))	
  (setq tlist (#sparser str " "))
  (setq strlst nil)
  (while (setq str (nth (setq n1 (1+ n1)) tlist))
    (if (/= " " str)
      (progn
	(setq tlist1 (#sparser str "="))
	(setq RP (- (length tlist1) 1))
	(setq n2 -1)
	(repeat RP
	  (setq str (strcat (nth (setq n2 (1+ n2)) tlist1) "=" (nth RP tlist1)))
	  (setq strlst (cons str strlst))		
	  )
	))
    );whiel-end
strlst
  )






;;;┏━━━━━━━━━━━━━━━━━━┓
;;;┃の 暂保留未列入使用范畴             ┃
;;;┗━━━━━━━━━━━━━━━━━━┛

;;;;※※※￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣※※※
;;;;※※※  tgh  水平排序 (固定间距)                      ※※※
;;;;※※※  tgv  垂直排序 (固定间距)                      ※※※
;;;;※※※________________________________________________※※※

(defun c:TQ1()(#t_jjdq 1))
(defun c:TQ2()(#t_jjdq 2))

(defun #t_jjdq(lx / jj ent t1 pt pt_x pt_y )
  (c:55)
  (while (not (setq ent (car (entsel "\n 选择文本")))))
  (setq pt (cdr (assoc 10 (entget ent))))
  (setq pt_x (car pt) pt_y (cadr pt))
  (cond	((= lx 1) (setq jj 4))
	((= lx 2) (setq jj (* (cdr (assoc 40 (entget ent))) 2) )))

  (while ent
    (setq t1 (cdr (assoc 1 (entget ent))))
    (cond
	((= lx 1) (setq pt_x (+ pt_x 5)))
	((= lx 2) (setq pt_y (- pt_y jj))) )
    (setq ent (car (entsel "\n 选择文本")))
    (sub_upd ent 10 (list pt_x pt_y))
    )
  (princ))

;|
(initget "Left Middle Right")                    
(setq ans (getkword "\n<Left>/Middle/Right: "))
(if (= ans nil) (setq ans "Left"))
(cond
  ((= ans "Left")   (setq w72 0))
  ((= ans "Middle") (setq w72 1))
  ((= ans "Right")  (setq w72 2))
) ;of cond


(defun c:TQhh()
  (c:55)
  (while (not (setq ent (car (entsel "\n 选择文本")))))
  (setq pt (cdr (assoc 10 (entget ent))))
  (setq pt_x (car pt) pt_y (cadr pt))
  (while ent
    (setq t1 (cdr (assoc 1 (entget ent))))
    (setq temp (+ (fix (* (strlen t1) 0.3469)) 2))
    (setq pt_x (+ pt_x temp))
    (setq ent (car (entsel "\n 选择文本")))
    (sub_upd ent 10 (list pt_x pt_y))
    )
  (princ))