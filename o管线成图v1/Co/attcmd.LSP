;;;;纪念明经处女贴发帖一周年 yjr111 2012-6-25
(defun C:attcmd (/ ss SS1 SS2 p0 h ang i ent ent1 str M textplst textlst sameset sameif
									option attoption newstr newcol newheight newang newstyle newlayer
									m attlst1 attlst2 spcae ENT1 ii atte wz1 Sign sameset std3 attlst
									oldattlst id3 th1 wz2 th2 wzs rwzs WZS_R oldosmode olddimzin m m
									dizengop copyorigent newAngletilt)
	(defun *error* (msg)
		(if (wcmatch (strcase msg t) "*break,*cancel*,*exit*,*取消*")
			(progn (princ "\n程序退出..."))
			(princ (strcat "\n" msg))
		)
		(princ)
	)
	(setvar "cmdecho" 0)
	(setq oldosmode(getvar "osmode"))
	(setq olddimzin(getvar 'dimzin))
	(setvar 'dimzin 8)
	(setvar "osmode" 0)
	(command "undo" "be")
	;;;;;;;;;;;;;;;;;;;;输入提示及保存函数;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(defun saveinputmsg (fc datastr msgstr1 msgstr2 notflag value / *bl* )
		(if (SETQ *bl* (VLAX-LDATA-GET "全局变量" datastr))
			(setq bl (fc (strcat msgstr1
										 "<"
										 (if (= (type *bl*) 'str)
											 *bl*
											 (if (= fc getangle)
                         (* (/ *bl* pi) 180)
                         (rtos *bl* 2 2)
											 )
										 )
										 ">："
									 )
							 )
			)
			(progn
				(setq *bl* value)
				(setq bl (fc (strcat msgstr1 msgstr2)))
			)
		)
		(if (= bl notflag)
			(setq bl *bl*)
			(setq *bl* bl)
		)
		(VLAX-LDATA-puT "全局变量" datastr *bl*)
		bl
	) 
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;进行输入交互;;;;;;;;;;;;;;;;;;;;;;;;;;
	(initget 7 "Add Totxt Backatt Replace Hide Show Q")
	(setq option(getkword"\n输入选项:[递增属性(Add)/转换为文本(Totxt)/转换回属性(Backatt)/更改属性值(Replace)/隐藏属性(Hide)/显示属性(Show)/帮助(Q)]"))
	(cond ((= option "Replace")
					(initget 7 "Text Color Height Width Rotate Angletilt Style Layer")
					(setq attoption(getkword"\n输入更改选项:[文本(Text)/颜色(Color)/高度(Height)/宽度(Width)/旋转(Rotate)/倾斜角(Angletilt)/字体样式(Style)/图层(Layer)]"))
					(cond((= attoption "Text")
								 (setq newstr(getstring "\n请输入属性替换文本"))
							 )
						((= attoption "Color")
              (setq newcol(saveinputmsg getint "newcol" "\n请输入属性替换颜色" "<1>" nil 1)) 
						)
						((= attoption "Height")
              (setq newheight(saveinputmsg getdist "newheight" "\n请输入属性替换高度" "<2.5>" nil 2.5))
						)
						((= attoption "Width")
              (setq newwid(saveinputmsg getreal "newwid" "\n请输入属性替换宽度系数" "<0.8>" nil 0.8))
						)
						((= attoption "Rotate")
              (setq newang(saveinputmsg getangle "newang" "\n请输入旋转角度" "<90.>" nil 90.))
						)
						((= attoption "Angletilt")
              (setq newAngletilt(saveinputmsg getangle "newAngletilt" "\n请输入倾斜角度" "<45.>" nil 45.))
						)
						((= attoption "Style")
							(setq newstyle(saveinputmsg getstring "newstyle" "\n请输入属性替换字体" "<HZTXT>" "" "HZTXT"))             
						)
						((= attoption "Layer")
							(setq newLAYER(saveinputmsg getstring "newLAYER" "\n请输入属性替换图层" "<0>" "" "0"))
						)
					)
				)
		((= option "Add")
			(INITGET "Copy Yes No")
			(setq dizengop(getkword"\n输入递增选项:[复制递增(Copy)/起始递增(Yes)/原位递增(No)]"))
			(if (not dizengop)(setq dizengop "No"))
			(cond((= dizengop "Yes")
						 (setq n(saveinputmsg getreal  "n" "\n请输入起始数字" "<1>" nil 1))
						 (setq m(saveinputmsg getreal  "m" "\n请输入递增数量" "<1>" nil 1))
					 )  
				((= dizengop "No")
					(setq m(saveinputmsg getreal  "m" "\n请输入递增数量" "<1>" nil 1))
					(setq n 1.)
				)
				((= dizengop "Copy")
	        (setq copyorigent (car(nentsel"\n选择复制递增的源属性文本")))
	        (if copyorigent(setq copyoristr(cdr(assoc 1(entget copyorigent)))))
					(setq m(saveinputmsg getreal  "m" "\n请输入递增数量" "<1>" nil 1))
					(setq n 1.)
				)
			)
		)
		((= option "Q")
			(helpmsg)
		)
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;程序开始;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(WHILE(and (/= option "Q")(princ"\n选择要进行操作的属性块或单行文本")(setq SS (ssget '((0 . "INSERT,TEXT")))));;;附带处理单行文本
		(setq SS1 (ssget "P" '((0 . "INSERT") (66 . 1))));;;过滤出属性块
		(if ss1(setq lstss1(paixuss ss1)))
		(vl-cmdf "SELECT" SS "")
		(SETQ SS2 (SSGET "P"));;;过滤出文本
		(if ss2 (progn(setq textlst(ss2lst ss2))(setq lstss2(paixuss ss2))))      
		(IF ss1;;;选到属性块
			(PROGN   
				(setq ii -1)
				(repeat(length lstss1)
					(setq attlst1 (list "标记             属性值");;;表头
						attlst2 (list "           ")            ;;;与attlst1表头对应空一行
						spcae "               "                 ;;;处理标记和属性值排列整齐度
          )
					(setq ENT1(entget (nth (setq ii (1+ ii))lstss1)))
					(while
						(= (cdr
								 (assoc 0
									 (setq ENT1 (entget (entnext (cdr (assoc -1 ENT1)))))
								 )
							 )
							"ATTRIB"
						)
						(setq atte (cdr (assoc -1 ENT1))
							wz1  (cdr (assoc 1 ENT1))
							Sign (cdr (assoc 2 ENT1))
						)
						(setq attlst1
							(append attlst1
								(list (list Sign (substr spcae (strlen sign)) wz1))
							)
						);;;标记和属性值组表以显示选择属性列表
						(setq attlst2 (cons atte attlst2));;;属性图元单独组表
					)
					(if(not sameset)(setq sameset "0"))
					(if (and (= sameset "0")
								(> (length attlst1)2)
							);;;属性不止1个出现对话框
						(progn
							;;;;;;;;;;;;;;对话框开始;;;;;;;;;;;;;;;;;;;
							(SETQ std3 3 )
							(WHILE (> std3 0)
								(attdetail)
								(COND ((= std3 5);;;取得所选属性值列表序号
												(setq attlst (str->lst attstr " "))
												(if (= (car attlst) "0")
													(setq attlst (cdr attlst))
												)
												(if (not attlst)
													(progn (princ "\n请勿单独选择第一行表头!") (exit))
												)
												(setq selectedsign
													(mapcar 'car
														(mapcar '(lambda (x) (nth (atoi x) attlst1))
															attlst
														)
													)
												);;;取得选择的标记
												(IF (or (< (length attlst1) (atoi (last attlst)))
															(< (length attlst1) (length attlst))
														)
													(setq oldattlst '("0"))
													(setq oldattlst ATTLST)
												)
											)
								)
							)
							(COND
								((= std3 0)
									(setq i 0)
									(repeat (length attlst)
										(if
											(member (car (setq newsign (nth (atoi (nth i attlst)) attlst1)))
												selectedsign
											);;;判断是否与取得的标记一致
											(attfuction
												(nth (atoi (nth i attlst)) (reverse attlst2))
												(last newsign)
											)
										)
										(setq i (1+ i))
									)
								)
								((= std3 -1) (unload_dialog id3) (term_dialog))
							)
							;;;;;;;;;;;;;;;;对话框结束;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						)
						(cond
							((<= (length attlst1) 2);;;属性仅1个不出现对话框
								(attfuction
									(nth 1 (reverse attlst2))
									(last (nth 1 attlst1))
								)
							)
							((and (= sameset "1") (> (length attlst1) 2));;;属性不止1个但已经选择不再显示时不出现对话框
								(setq attlst (str->lst attstr " "))
								(if (= (car attlst) "0")
									(setq attlst (cdr attlst))
								)
								(if (not attlst)
									(progn (princ "\n请勿单独选择第一行表头!") (exit))
								)
								(setq i 0)
								(repeat (length attlst)
									(if
										(member (car (setq newsign (nth (atoi (nth i attlst)) attlst1)))
											selectedsign
										);;;判断是否与取得的标记一致
										(attfuction
											(nth (atoi (nth i attlst)) (reverse attlst2))
											(last newsign)
										)
									)
									(setq i (1+ i))
								)
							)
						)
					)
				)
			)
		)
		(IF ss2;;;选到文本
			(PROGN
				(setq I -1)
				(repeat (length lstss2)
					(setq ENT1 (entget (nth(setq I (1+ I))lstss2)))
					wz1  (cdr (assoc 1 ENT1))
				)
				(cond ((= option "Add")
								(cutstring wz1)
								(dizengmode)
								(entmod ENT1)
								(ENTUPD (CDR (ASSOC -1 ENT1)))
								(setq n   (+ n m)
									*n* n
								)
							)
					((= option "Replace")
						(replaceatt ent1 )
					)
				)
			)
		)
  )
	(command "undo" "e")
	(setvar "osmode" oldosmode)
	(setvar 'dimzin olddimzin)
	(princ)
)
;;;;;;;;;;;;;;;;;;;属性操作主程序;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun attfuction(e wz1 / align point textstr dist textplst texte)
	(setq ent1(entget e))
	(cond((= option "Add");;;属性递增
				 (cutstring wz1)                 
				 (dizengmode)
				 (entmod ENT1)
				 (ENTUPD (CDR(ASSOC -1 ENT1)))
				 (showorhideatt ent1 0)
				 (setq n (+ n m) *n* n)
			 )
		((= option "Totxt");;;转换成文本
			(textalignmode ent1)
			(Command "_.TEXT"
				"_S"
				(cdr (assoc 7 ENT1))
				"j"
				align
				point
				(cdr (assoc 40 ENT1))
				(* (/ (cdr (assoc 50 ENT1)) pi) 180)
				wz1
			)
			(setq ent2(entget(setq e2(entlast))))
			(setq ent2 (subst (assoc 41 ENT1) (assoc 41 ent2)ent2))
			(entmod ent2)
			(entupd e2)
			(setq newstr "")
			(replaceatt ent1)
			(showorhideatt ent1 1)
			(setq textplst nil)
		)
		((= option "Backatt");;;转换回属性
			(foreach x textlst
				(setq s        (entget x)
					textstr  (cdr (assoc 1 s))
					dist     (distance (cdr (assoc 10 ent1))
										 (setq textp (cdr (assoc 10 s)))
									 )
					textplst (append (list (list dist textstr x)) textplst)
				)                  
			)
			(setq textplst
				(vl-sort textplst
					'(lambda (x y) (< (car x) (car y)))
				)
			)
			(setq newstr(cadar textplst))
			(setq texte(caddar textplst))
			(replaceatt ent1)
			(showorhideatt ent1 0)
			(if texte
				(progn
					(entdel texte )
					(setq textlst(vl-remove texte textlst))
				)
			)
		)
		((= option "Replace");;;替换属性内容
			(showorhideatt ent1 0)
			(replaceatt ent1)
		)
		((= option "Hide");;;隐藏属性内容
			(showorhideatt ent1 1)
		)
		((= option "Show");;;显示属性内容
			(showorhideatt ent1 0)
		)
	)
)
;;;;;;;;;;;;;;;;;;;;;;递增模式子函数;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dizengmode()
	(cond((= dizengop "Yes")
				 (setq ENT1  (subst (cons 1 (strcat wz2 RWZS)) (assoc 1 ENT1) ENT1))
			 )
		((= dizengop "No")
			(setq ENT1  (subst (cons 1 (strcat wz2 (rtos (+ m wzs_r)2 2))) (assoc 1 ENT1) ENT1))
		)
		((= dizengop "Copy")
			(cutstring copyoristr)
			(setq ENT1  (subst (cons 1 (if (/= m 0)(strcat wz2 (rtos (+ m wzs_r)2 2))copyoristr)) (assoc 1 ENT1) ENT1))
			(if (/= m 0)(setq m(1+ m))m)
		)
	)
)
;;;;;;;;;;;;;;;;;截取文字末尾字符;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cutstring(str / th1 th2 )
	(setq th1   (strlen str)
		wz2   (vl-string-right-trim "1234567890." str)
		th2   (strlen wz2)
		wzs   (substr str (+ th2 1) (- th1 th2))
		rwzs (rtos n 2 5)
		WZS_R (aTOF WZS)
	)
)
;;;;;;;;;;;;;;;;;属性文字对齐方式;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;[对齐(A)/调整(F)/中心(C)/中间(M)/右(R)/左上(TL)/中上(TC)/右上(TR)/左中(ML)/正中(MC)
;;;/右中(MR)/左下(BL)/中下(BC)/右下(BR)]
(defun textalignmode (s / levalalign veralign p10 p11)
	(setq levalalign (cdr (assoc 72 s))
		veralign   (cdr (assoc 74 s))
		p10        (cdr (assoc 10 s))
		p11        (cdr (assoc 11 s))
	)
	(setq align (nth (vl-position
										 (list levalalign veralign)
										 '((0 0)
												(0 1)
												(0 2)
												(0 3)
												(1 0)
												(1 1)
												(1 2)
												(1 3)
												(2 0)
												(2 1)
												(2 2)
												(2 3)
												(3 0)
												(4 0)
												(5 0)
											)
									 )
								'("bl"   "bl"   "ml"   "tl"   "c"    "bc"   "mc"
									 "tc"   "r"    "br"   "mr"   "tr"   "bl"   "m"
									 "r"
								 )
							)
	)
	(if (= (list levalalign veralign) '(0 0))
		(setq point p10)
		(setq point p11)
	)
)
;;;;;;;;;;;;;;;;;显示隐藏属性值;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;flag--1 隐藏属性
;;;flag--0 显示属性
(defun showorhideatt(ent1 flag)
	(if (and ent1 flag)
		(progn
			(setq ENT1 (subst (cons 70 flag) (assoc 70 ENT1) ENT1))
			(entmod ENT1)             
			(ENTUPD (CDR(ASSOC -1 ENT1)))
		)
  )
)
;;;;;;;;;;;;;;;;;替换属性值及特性值;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun replaceatt (ent1 / optionmodelst optionmodedxflst newprolst)
	(setq optionmodelst
		'("Text" "Color" "Height" "Width" "Rotate" "Angletilt" "Style" "Layer")
	)
	(setq optionmodedxflst '(1 62 40 41 50 51 7 8)) 
	(setq
		newprolst '(newstr newcol newheight newwid newang newAngletilt newstyle newlayer)
	)
	(mapcar '(lambda (x y z)
						 (if (= attoption x)
							 (setq ENT1 (subst (cons y (eval z)) (assoc y ENT1) ENT1))
						 )           
					 )
		optionmodelst
		optionmodedxflst
		newprolst
	)
	(if (and(= attoption "Color")(=(assoc 62 ENT1)nil))
    (vla-put-color (vlax-ename->vla-object(cdr(assoc -1 ENT1)))newcol)        
	)
	(entmod ENT1)
	(ENTUPD (CDR (ASSOC -1 ENT1)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;属性对话框程序;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun attdetail(/ fn3 lsdcl3 )
	(setq fn3 (open (setq lsdcl3 (VL-FILENAME-MKTEMP "tmp" "" ".dcl")) "w"))
	(if (not attstr)
		(setq attstr "0")
		(progn
			(setq attstr "")
			(mapcar '(lambda(x)(setq attstr (strcat  attstr  x " "))) oldattlst)
		)
	)
	(write-line "att:dialog{" fn3)
	(write-line "   label=\"属性选择\";" fn3)
	(write-line "   :boxed_row{label=\"选择属性\";" fn3)
	(write-line "        :list_box{key=\"attlst\";fixed_width=true;width=41;multiple_select=true;}" fn3)
	(write-line "	}" fn3) 
	(write-line "   :row{" fn3)
	(write-line "        :toggle{label=\"其余均按此选择\";key=\"sameset\";fixed_width=true;width=8;}" fn3)
	(write-line "        :button{label=\"确定\";key=\"accept\";fixed_width=true;width=4;}" fn3)
	(write-line "        :button{label=\"取消\";key=\"cancel\";is_cancel=true;fixed_width=true;width=4;}" fn3)
	(write-line "	}" fn3)
	(write-line "	}" fn3)
	(close fn3)
	(setq id3 (LOAD_DIALOG lsdcl3))
	(new_dialog "att" id3 "" screenpt3)
	(start_list "attlst" 2);;;处理列表开始
	(mapcar 'add_list (mapcar '(lambda(x)(vl-string-trim "()"(vl-princ-to-string x))) attlst1));;;将选定的名称逐个添加到列表
	(end_list);;;添加列表结束
	(set_tile "attlst" attstr)
	(set_tile "sameset" sameset)
	(action_tile "attlst" "(setq attstr $value)(setq screenpt3(done_dialog 5))");;;属性列表
	(action_tile "sameset" "(setq sameset $value)(setq screenpt3(done_dialog 6))")
	(action_tile "accept" "(setq screenpt3(done_dialog))");;;确定
	(action_tile "cancel" "(setq screenpt3(done_dialog -1))");;;取消
	(setq std3(START_DIALOG))
	(unload_dialog id3)
	(VL-FILE-DELETE lsdcl3)
	(if lsdcl3(VL-FILE-DELETE lsdcl3))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;帮助信息;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun helpmsg()
	(alert(strcat"\n                          使用说明\n"
					"\n\n1、 本程序主要对属性块的属性文本的一些快捷操作。"
					"\n\n2、 递增属性需要输入起始数字及递增量，程序自动在属性文本末尾递增。"
					"\n\n3、 转换为文本指将属性在同一位置转换为文本，多属性时会弹出对话框"
					"\n\n    要求选择相应的属性后操作。"
					"\n\n4、 转换回属性仅在块为属性块的基础上进行，相当于转换为文本的逆操作。"
					"\n\n    注意操作时要有一一对应关系，否则出错。"
					"\n\n5、 递增和更改功能单行文本也适用。"
					"\n\n6、 其他功能请自行试用!")
	)
)
;;;;选择集转表
(defun ss2lst (ss / i e lst)
	(setq i -1)
	(repeat (sslength ss)
		(setq e (ssname ss (setq i (1+ i)))
			lst (cons e lst)
		)
	)
)
;;;str:字符串 sign字符串分割标记，例如"1 2 3 4"->("1" "2" "3" "4")
(defun str->lst(str sign / position lst)
	(while (and str(/= str ""))
		(if(setq position (vl-string-search sign str))
      (progn
				(setq lst (append lst (list (substr str 1  position))))
				(setq str (substr str  (+ 2 position)))
      )
      (progn
				(setq lst (append lst (list  str )))
				(setq str nil)
      )
		)
	)
	lst
)
;;;;;;;;;;;;;按左右上下顺序排序选择集图元;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun paixuss(ss / lst)
	(setq lst (ss2lst ss))
	(setq lst(vl-sort lst '(lambda(x y)(if (equal (cadr(cdr(assoc 10 (entget x))))(cadr(cdr(assoc 10 (entget y))))1e-1)
																			 (<(car(cdr(assoc 10 (entget x))))(car(cdr(assoc 10 (entget y)))))
																		 )
												 )
					 )
	)
	lst
)
(princ"\n属性块属性工具，by yjr111 2012-6-23.命令：attcmd")
(princ"\n")