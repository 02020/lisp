(defun Whether-The-User-Has-Permissions	(/	      User-List
					 UsersPath    shell
					 ComputerName UserName
					 UserProFile  UserDomain
					 AllUser      xx
					)
  (setq	User-List (mapcar 'strcase
			  '("s00292"	 "s00295"     "s00318"
			    "s00341"	 "s00376"     "s00449"
			    "s00778"	 "s04927"     "s05388"
			    "s05390"	 "s06759"     "s07148"
			   )
		  )
	UsersPath ""
  )
  (setq shell (vlax-create-object "WScript.Shell"))
  (setq	ComputerName
	 (vlax-invoke-method
	   shell
	   'ExpandEnvironmentStrings
	   "%ComputerName%"
	 )
  )
  (setq	UserName (vlax-invoke-method
		   shell
		   'ExpandEnvironmentStrings
		   "%UserName%"
		 )
  )
  (setq	UserProFile
	 (vlax-invoke-method
	   shell
	   'ExpandEnvironmentStrings
	   "%UserProFile%"
	 )
  )
  (setq	UserDomain
	 (vlax-invoke-method
	   shell
	   'ExpandEnvironmentStrings
	   "%UserDomain%"
	 )
  )
  (vlax-release-object shell)
  (setq	UsersPath (vl-string-subst
		    ""
		    (strcat "\\" (strcase UserName))
		    (strcase UserProFile)
		  )
  )
  (setq AllUser (vl-directory-files UsersPath nil -1))
  (setq UserRoot nil)
  (foreach xx AllUser
    (if	(member (strcase xx) User-List)
      (setq UserRoot T)
    )
  )
  (if (eq (strcase UserDomain) "ZHSYE")
    (if	UserRoot
      T
      nil
    )
    T
  )
)

(if (null (Whether-The-User-Has-Permissions))
  (exit)
)

(defun Entmake-attblock-dcl (filedata nn / ffff)
  (cond
    ((setq ffff (open filedata "w"))
     (if (< 24 nn)
       (setq nn 24)
     )
     (princ "block_replace:dialog { \n" ffff)
     (princ
       "	label=\"属性块替换\";key=\"shi-jian\";alignment=centered;\n"
       ffff
     )
     (princ "	:row{ \n" ffff)
     (princ "		:boxed_column{ \n" ffff)
     (princ "			label=\"原图属性值\"; \n" ffff)
     (princ
       "				:concatenation{:text{label=\"块名称:\";}:text{label=\"\";key=\"Old-block-name\";width=10;}:text{label=\"块比例:\";}:text{label=\"\";key=\"Old-block-Scale\";width=5;}} \n"
       ffff
     )
     (princ (strcat
	      "				:column {:list_box{key=\"Old-value\";width=30;height="
	      (rtos (+ nn 2))
	      ";fixed_width_fint=true;}} \n"
	    )
	    ffff
     )
     (princ "				}\n" ffff)
     (princ "		:boxed_column{ \n" ffff)
     (princ "		label=\"新图属性值\"; \n" ffff)
     (princ
       "			:concatenation{:text{label=\"块名称:\";}:text{label=\"\";key=\"New-block-name\";width=10;}:text{label=\"块比例:\";}:text{label=\"\";key=\"New-block-Scale\";width=5;}} \n"
       ffff
     )
     (princ (strcat
	      "				:column {:list_box{key=\"New-value\";width=30;height="
	      (rtos (+ nn 2))
	      ";fixed_width_fint=true;}} \n"
	    )
	    ffff
     )
     (princ "			}\n" ffff)
     (princ "	}\n" ffff)
     (princ "				:concatenation{ \n" ffff)
     (princ
       "						:button{label=\"上移(&O)\";key = \"Move-up\";width=4;} \n"
       ffff
     )
     (princ
       "						:button{label=\"下移(&D)\";key = \"Move-down\";width=4;} \n"
       ffff
     )
     (princ
       "						:button{label=\"删除(&R)\";key = \"Move-Delete\";width=4;} \n"
       ffff
     )
     (princ
       "						:button{label=\"颠倒(&E)\";key = \"Upside-down\";width=4;} \n"
       ffff
     )
     (princ
       "						:button{label=\"倒置(&C)\";key = \"Upside-C\";width=4;} \n"
       ffff
     )
     (princ
       "						:button{label=\"后退(&B)\";key = \"Upside-bock\";width=4;} \n"
       ffff
     )
     (princ "					}\n" ffff)
     (princ "		:concatenation{ \n" ffff)
     (princ "			:toggle{ \n" ffff)
     (princ "				label=\"中心点插入\"; \n" ffff)
     (princ "				key=\"Button-1\";width=32.5; \n" ffff)
     (princ "			}\n" ffff)
     (princ "			:toggle{ \n" ffff)
     (princ "				label=\"属性值嫁接\"; \n" ffff)
     (princ "				key=\"Button-4\";width=32.5; \n" ffff)
     (princ "			}\n" ffff)
     (princ "			:toggle{ \n" ffff)
     (princ "				label=\"删除原块\"; \n" ffff)
     (princ "				key=\"Button-6\"; \n" ffff)
     (princ "			}\n" ffff)
     (princ "		}\n" ffff)
     (princ "		:concatenation{ \n" ffff)
     (princ "			:toggle{ \n" ffff)
     (princ "				label=\"忽略块比例\";width=10; \n" ffff)
     (princ "				key=\"Button-2\";width=15; \n" ffff)
     (princ "			}\n" ffff)
     (princ "			:edit_box{ \n" ffff)
     (princ "				label=\"缩放比例(新图/原图)\";width=15; \n"
	    ffff
     )
     (princ "				edit_width=10; \n" ffff)
     (princ "				key=\"proportion\";width=15; \n" ffff)
     (princ "			}\n" ffff)
     (princ "			:popup_list{ \n" ffff)
     (princ "				key=\"proportion-list\";width=15; \n" ffff)
     (princ "				edit_width=10; \n" ffff)
     (princ "			}\n" ffff)
     (princ "			:text{label=\"   倍\";width=5;} \n" ffff)
     (princ "		}\n" ffff)
     (princ "		:concatenation{ \n" ffff)
     (princ "			:toggle{ \n" ffff)
     (princ "				label=\"忽略块角度\";width=10; \n" ffff)
     (princ "				key=\"Button-3\";width=15; \n" ffff)
     (princ "			}\n" ffff)
     (princ "			:edit_box{ \n" ffff)
     (princ "				label=\"角度差值(新图-原图)\";width=15; \n"
	    ffff
     )
     (princ "				edit_width=10; \n" ffff)
     (princ "				key=\"Angle-difference\";width=15; \n" ffff)
     (princ "			}\n" ffff)
     (princ "			:popup_list{ \n" ffff)
     (princ "				key=\"Angle-list\";width=15; \n" ffff)
     (princ "				edit_width=10; \n" ffff)
     (princ "			}\n" ffff)
     (princ "			:text{label=\"   度\";width=5;} \n" ffff)
     (princ "		}\n" ffff)
     (princ "		:concatenation{ \n" ffff)
     (princ "			:toggle{ \n" ffff)
     (princ "				label=\"忽略值角度\"; \n" ffff)
     (princ "				key=\"Button-5\"; \n" ffff)
     (princ "			}\n" ffff)
     (princ "			:edit_box{ \n" ffff)
     (princ "				label=\"角度差值(新图-原图)\"; \n" ffff)
     (princ "				edit_width=10; \n" ffff)
     (princ "				key=\"Angle-Attribute\";\n" ffff)
     (princ "			}\n" ffff)
     (princ "			:popup_list{ \n" ffff)
     (princ "				key=\"Angle-Attribute-list\";width=15; \n" ffff)
     (princ "				edit_width=10; \n" ffff)
     (princ "			}\n" ffff)
     (princ "			:text{label=\"   度\";width=5;} \n" ffff)
     (princ "		}\n" ffff)
     (princ "		:row{\n" ffff)
     (princ "			spacer_1;spacer_1;spacer_1;spacer_1;\n" ffff)
     (princ
       "			:button{label=\"确定(&Y)\";key=\"accept\";is_default=true;fixed_width=true;}\n"
       ffff
     )
     (princ
       "			:button{label=\"取消(&N)\";key=\"cancel\";fixed_width=true;is_cancel=true;}\n"
       ffff
     )
     (princ
       "			:button{label=\"帮助(&H)\";key=\"help\";fixed_width=true;}\n"
       ffff
     )
     (princ "			spacer_1;spacer_1;spacer_1;spacer_1;\n" ffff)
     (princ "		}\n" ffff)
     (princ "} \n" ffff)
     (close ffff)
     (princ)
    )
    (t nil)
  )
					;left,centered,right,top,centered,bottom
					;左，居中，右，上，中心，底部
)
(defun string->itom-int	(str)
  (cond	((or (eq (type str) 'INT) (eq (type str) 'REAL)) str)
	(t
	 (setq
	   Symbol-by (if (setq position (vl-string-search "/" str))
		       '/
		       (if (setq position (vl-string-search "*" str))
			 '*
			 (if (setq position (vl-string-search "+" str))
			   '+
			   (if (setq position (vl-string-search "-" str))
			     '-
			     str
			   )
			 )
		       )
		     )
	 )
	 (cond ((= Symbol-by str) (atof str))
	       (t
		((eval Symbol-by)
		  (atof (substr str 1 position))
		  (atof (substr str (+ position 2) (strlen str)))
		)
	       )
	 )
	)
  )
)
(defun ssget-dxf66-1 (system32 / tk)
  (while (null (and
		 (setq tk (car (eval system32)))
		 (if tk
		   (= 1 (cdr (assoc 66 (entget tk))))
		   nil
		 )
	       )
	 )
  )
  tk
)
;;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defun c:ATBLOCK (/		     *error*
		  get-Attribute-value-list-n
		  get-Attribute-value
		  n		     list-sort-n-n
		  show-list	     verification-Attribute
		  quantity-Equal     ss
		  DCL-file	     New-block
		  Old-block	     New-blockname
		  Old-blockname	     New-Attribute-value
		  Old-Attribute-value
		  Serial-number	     Operation-number
		  DCL-ID	     New-number-list
		  Old-number-list    mode-tile-Button
		  N-N		     O-N
		  OBJ1		     OBJ2
		  PT1		     Button-1
		  Button-2	     Button-3
		  Button-4	     Button-5
		  Button-6	     Atblock-Record-Data
		  Angle-Attribute    Obj-block
		  proportion-list    proportion
		  Angle-difference   Angle-list
		  Dictionaries-list2 Angle-Attribute-list
		  Button-7	     Obj1-text
		 )
  (setvar "dimzin" 8)
  (defun *error* (msg)
    (if	(wcmatch (strcase msg t) "*break,*cancel*,*exit*,*取消*")
      (princ)
      (vla-EndUndoMark
	(vla-get-ActiveDocument (vlax-get-acad-object))
      )
    )
    (princ)
  )
  (defun get-Attribute-value-list-n (obj nn / objlist nlist n)
    (setq obj	  (if (= (type obj) 'ENAME)
		    (vlax-ename->vla-object obj)
		    obj
		  )
	  objlist (vlax-safearray->list
		    (vlax-variant-value (vla-getattributes obj))
		  )
	  n	  nn
    )
    (foreach obj objlist
      (setq nlist (append
		    nlist
		    (list (strcat (rtos n) "、" (vla-get-TextString obj)))
		  )
	    n	  (1+ n)
      )
    )
    nlist
  )
  (defun get-Attribute-value (obj nn / objlist nlist)
    (setq obj	  (if (= (type obj) 'ENAME)
		    (vlax-ename->vla-object obj)
		    obj
		  )
	  objlist (vlax-safearray->list
		    (vlax-variant-value (vla-getattributes obj))
		  )
    )
    (foreach obj objlist
      (setq nlist (append nlist (list ((eval nn) obj))))
    )
    nlist
  )
  (defun list-sort-n-n (n1 lst / nmax n-q n-h l-hou l-qian nlst)
    (setq nmax (vl-list-length lst))
    (cond ((and (> n1 0) (< n1 nmax))
	   (setq n-q	(nth (1- n1) lst)
		 n-h	(nth n1 lst)
		 l-hou	(cdr (member n-h lst))
		 l-qian	(reverse (cdr (member n-q (reverse lst))))
	   )
	   (setq nlst (append l-qian (list n-h) (list n-q) l-hou))
	  )
	  (t (setq nlst lst))
    )
    nlst
  )
  (defun show-list (key nlist)
    (start_list key)
    (mapcar 'add_list nlist)
    (end_list)
  )
  (defun verification-Attribute	(obj		  /
				 New-blockname	  o-Attribute-list
				 nobj		  n-Attribute-list
				 *AcadDocument*	  *mSpace*
				)
    (setq obj (if (= (type obj) 'ENAME)
		(vlax-ename->vla-object obj)
		obj
	      )
    )
    (if	(vlax-property-available-p obj 'HasAttributes)
      (cond
	((= (vla-get-HasAttributes obj) :vlax-true)
	 (setq *AcadDocument*	(vla-get-ActiveDocument
				  (vlax-get-acad-object)
				)
	       *mSpace*		(vla-get-ModelSpace *AcadDocument*)
	       New-blockname	(vla-get-Name obj)
	       o-Attribute-list	(vlax-safearray->list
				  (vlax-variant-value
				    (vla-getattributes Obj)
				  )
				)
	       nobj		(vla-insertblock
				  *mSpace*
				  (vla-get-InsertionPoint obj)
				  New-blockname
				  1
				  1
				  1
				  0
				)
	       n-Attribute-list	(vlax-safearray->list
				  (vlax-variant-value
				    (vla-getattributes nobj)
				  )
				)
	 )
	 (vla-delete nobj)
	 (cond
	   ((= (length o-Attribute-list) (length n-Attribute-list)) t)
	   (t
	    (vlax-invoke-method
	      (vlax-create-object "wscript.shell")
	      'Popup
	      "图框的属性列表有错误，\n请使用 BATTMAN 命令同步此属性块之后再试。"
	      10
	      "提示"
	      16
	    )
	    nil
	   )

	 )
	)
      )
      nil
    )
  )
  (defun quantity-Equal	(lst1 lst2 / New-number Old-number)
    (repeat (min (length lst1) (length lst2))
      (setq New-number (append New-number (list (car lst1)))
	    Old-number (append Old-number (list (car lst2)))
	    lst1       (cdr lst1)
	    lst2       (cdr lst2)
      )
    )
    (list New-number Old-number)
  )
  (setq
    DCL-file (strcat (vlax-get-property (vlax-get-acad-object) 'Path)
		     "\\block-replace.dcl"
	     )
  )
  (setq
    New-block (vlax-ename->vla-object
		(ssget-dxf66-1 '(entsel "\n选择用来替换的属性块:"))
	      )
  )
  (if (null (verification-Attribute New-block))
    (exit)
  )
  (setq	Old-block (vlax-ename->vla-object
		    (ssget-dxf66-1 '(entsel "\n选择被替换的属性块:"))
		  )
  )
  (if (null (verification-Attribute Old-block))
    (exit)
  )
  (setq	New-Attribute-value
	 (get-Attribute-value-list-n New-block 1)
	Old-Attribute-value
	 (get-Attribute-value-list-n Old-block 1)
	New-0ld	1
	Serial-number 0
	Operation-number
	 0
	New-blockname
	 (vla-get-Name New-block)
	Old-blockname
	 (vla-get-Name Old-block)
	N-N (1+ (length (get-Attribute-value-list-n New-block 1)))
	O-N (1+ (length (get-Attribute-value-list-n Old-block 1)))
  )
  (Entmake-attblock-dcl
    DCL-file
    (max (length New-Attribute-value)
	 (length Old-Attribute-value)
    )
  )
  (setq DCL-ID (load_dialog "block-replace.dcl"))
  (if (not (new_dialog "block_replace" DCL-ID))
    (exit)
  )
  (set_tile
    "shi-jian"
    (strcat "属性块替换          "
	    (MENUCMD "M=$(EDTIME,$(GETVAR,DATE),YYYY.MO.DD HH:MM DDDD)")
	    "          易 QQ:1357068078"
    )
  )
  (vl-file-delete DCL-file)
  (set_tile "New-block-Scale"
	    (rtos (vla-get-XScaleFactor New-block))
  )
  (set_tile "Old-block-Scale"
	    (rtos (vla-get-XScaleFactor Old-block))
  )
  (show-list "New-value" New-Attribute-value)
  (show-list "Old-value" Old-Attribute-value)
  (show-list "proportion-list"
	     (setq proportion-list
		    (mapcar '(lambda (xx) (rtos xx 2 2))
			    '(0.1   0.2	  0.3	0.4   0.5   0.6	  0.7
			      0.8   0.9	  1.0	1.5   2.0   2.5	  3.0
			      3.5   4.0	  4.5	5.0   6.0   7.0	  8.0
			      9.0   10.0
			     )
		    )
	     )
  )
  (show-list "Angle-list"
	     (setq Angle-list
		    (mapcar '(lambda (xx) (rtos xx 2 2))
			    '(0 30 45 60 90 135 180 270)
		    )
	     )
  )
  (show-list "Angle-Attribute-list"
	     (setq Angle-Attribute-list
		    (mapcar '(lambda (xx) (rtos xx 2 2))
			    '(0 30 45 60 90 135 180 270)
		    )
	     )
  )
  (set_tile "New-value" (rtos Serial-number))
  (set_tile "Old-value" (rtos Serial-number))
  (mapcar 'set_tile
	  '("proportion"	     "proportion-list"
	    "Angle-difference"	     "Angle-list"
	    "Angle-Attribute"	     "Angle-Attribute-list"
	   )
	  '("1" "9" "0" "0" "0" "0")
  )
  (mapcar 'mode_tile
	  '("Move-up"		 "Move-down"
	    "Move-Delete"	 "proportion"
	    "proportion-list"	 "Angle-Attribute"
	   )
	  '(1 1 1 0 0 0)
  )
  (set_tile "New-block-name" New-blockname)
  (set_tile "Old-block-name" Old-blockname)
  (defun Record-Dictionaries ()
    (vlax-ldata-put
      "Atblock-Record-Data"
      "Atblock"
      (list Operation-number
	    New-Attribute-value
	    Old-Attribute-value
	    New-0ld
      )
    )
    (vlax-ldata-put
      "Atblock-Record-Data"
      (rtos Operation-number)
      (list Serial-number
	    New-Attribute-value
	    Old-Attribute-value
	    New-0ld
      )
    )
    (setq Operation-number (1+ Operation-number))
  )
  (defun Operation-Dictionaries	(/ Dictionaries-list)
    (setq Dictionaries-list
	   (vlax-ldata-get
	     "Atblock-Record-Data"
	     (rtos (1- Operation-number))
	   )
    )
    (vlax-ldata-put
      "Atblock-Record-Data"
      "Atblock"
      (list Operation-number
	    New-Attribute-value
	    Old-Attribute-value
	    New-0ld
      )
    )
    (vlax-ldata-delete
      "Atblock-Record-Data"
      (rtos Operation-number)
    )
    (setq Operation-number
	   (if (< 0 Operation-number)
	     (1- Operation-number)
	     Operation-number
	   )
	  Serial-number
	   (if Dictionaries-list
	     (nth 0 Dictionaries-list)
	     Serial-number
	   )
	  New-Attribute-value
	   (if Dictionaries-list
	     (nth 1 Dictionaries-list)
	     New-Attribute-value
	   )
	  Old-Attribute-value
	   (if Dictionaries-list
	     (nth 2 Dictionaries-list)
	     Old-Attribute-value
	   )
	  New-0ld (if Dictionaries-list
		    (nth 3 Dictionaries-list)
		    New-0ld
		  )
    )
  )
  (Record-Dictionaries)
  (set_tile "Button-6" "1")
  (defun mode-tile-Button (vl lst n b1 b2 b3 b4)
    (show-list vl lst)
    (set_tile vl (rtos n))
    (cond (lst
	   (mode_tile b3 0)
	   (mode_tile b4 0)
	   (if (= n 0)
	     (mode_tile b1 1)
	     (mode_tile b1 0)
	   )
	   (if (= n (1- (length lst)))
	     (mode_tile b2 1)
	     (mode_tile b2 0)
	   )
	  )
	  (t (mapcar 'mode_tile (list b1 b2 b3 b4) '(1 1 1 1)))
    )
  )
  (defun Button-2-3 ()
    (cond ((= "0" (get_tile "Button-2"))
	   (mode_tile "proportion-list" 0)
	   (mode_tile "proportion" 0)
	  )
	  (t
	   (mode_tile "proportion-list" 1)
	   (mode_tile "proportion" 1)
	  )
    )
    (cond ((= "0" (get_tile "Button-3"))
	   (mode_tile "Angle-list" 0)
	   (mode_tile "Angle-difference" 0)
	  )
	  (t
	   (mode_tile "Angle-list" 1)
	   (mode_tile "Angle-difference" 1)
	  )
    )
    (cond ((= "0" (get_tile "Button-5"))
	   (mode_tile "Angle-Attribute-list" 0)
	   (mode_tile "Angle-Attribute" 0)
	  )
	  (t
	   (mode_tile "Angle-Attribute-list" 1)
	   (mode_tile "Angle-Attribute" 1)
	  )
    )
  )
  (defun Move-Delete-Reason ()
    (Record-Dictionaries)
    (cond ((= New-0ld 1)
	   (setq New-Attribute-value
		  (vl-remove
		    (nth Serial-number New-Attribute-value)
		    New-Attribute-value
		  )
		 Serial-number
		  (if
		    (= (length New-Attribute-value) Serial-number)
		     (1- Serial-number)
		     Serial-number
		  )
	   )
	   (mode-tile-Button
	     "New-value"	 New-Attribute-value
	     Serial-number	 "Move-up"
	     "Move-down"	 "Move-Delete"
	     "Upside-down"
	    )
	  )
	  (t
	   (setq Old-Attribute-value
		  (vl-remove
		    (nth Serial-number Old-Attribute-value)
		    Old-Attribute-value
		  )
		 Serial-number
		  (if
		    (= (length Old-Attribute-value) Serial-number)
		     (1- Serial-number)
		     Serial-number
		  )
	   )
	   (mode-tile-Button
	     "Old-value"	 Old-Attribute-value
	     Serial-number	 "Move-up"
	     "Move-down"	 "Move-Delete"
	     "Upside-down"
	    )
	  )
    )
  )
  (action_tile "Button-2" "(Button-2-3)")
  (action_tile "Button-3" "(Button-2-3)")
  (action_tile "Button-5" "(Button-2-3)")
  (action_tile
    "Button-4"
    "(cond	(	(= \"1\" (get_tile \"Button-4\"))(setq Button-1 (get_tile \"Button-1\") Button-2 (get_tile \"Button-2\") Button-3 (get_tile \"Button-3\") Button-5 (get_tile \"Button-5\") Button-6 (get_tile \"Button-6\"))
(mapcar	'mode_tile	(list	\"Button-1\" \"Button-2\" \"Button-3\"  \"Button-5\"  \"Button-6\" \"proportion-list\" \"proportion\" \"Angle-Attribute\" \"Angle-Attribute-list\" \"Angle-difference\" \"Angle-list\")'(1 1 1 1 1 1 1 1 1 1 1))
(mapcar 'set_tile	(list	\"Button-1\" \"Button-2\" \"Button-3\" \"Button-5\" \"Button-6\")	'(\"0\" \"0\" \"0\" \"0\" \"0\"))
)(t	(mapcar	'mode_tile	(list	\"Button-1\" \"Button-2\" \"Button-3\"  \"Button-5\"  \"Button-6\" \"proportion-list\" \"proportion\" \"Angle-Attribute\" \"Angle-Attribute-list\" \"Angle-difference\" \"Angle-list\")'(0 0 0 0 0 0 0 0 0 0 0))
(mapcar 'set_tile	(list	\"Button-1\" \"Button-2\" \"Button-3\" \"Button-5\" \"Button-6\")	(list	Button-1 Button-2 Button-3 Button-5 Button-6))(Button-2-3)
)	)
" )
  (action_tile
    "proportion-list"
    "(set_tile \"proportion\" (nth (atoi $value) proportion-list))"
  )
  (action_tile
    "Angle-list"
    "(set_tile \"Angle-difference\" (nth (atoi $value) Angle-list))"
  )
  (action_tile
    "Angle-Attribute-list"
    "(set_tile \"Angle-Attribute\" (nth (atoi $value) Angle-Attribute-list))"
  )
  (action_tile
    "New-value"
    "(setq Serial-number (atoi $value) New-0ld 1) (mode-tile-Button \"New-value\" New-Attribute-value Serial-number \"Move-up\" \"Move-down\" \"Move-Delete\" \"Upside-down\")(if (eq $reason 4) (Move-Delete-Reason))"
  )
  (action_tile
    "Old-value"
    "(setq Serial-number (atoi $value) New-0ld 2) (mode-tile-Button \"Old-value\" Old-Attribute-value Serial-number \"Move-up\" \"Move-down\" \"Move-Delete\" \"Upside-down\")(if (eq $reason 4) (Move-Delete-Reason))"
  )
  (action_tile
    "Upside-C"
    "(Record-Dictionaries)
(cond	(	(=	New-0ld	1)
(show-list	\"New-value\"	(setq New-Attribute-value (mapcar '(lambda(xx) (strcat (rtos (- N-N (atoi (substr xx 1 (vl-string-search \"、\" xx))))) (substr xx (1+ (vl-string-search \"、\" xx))))) New-Attribute-value)))
(set_tile	\"New-value\"	(rtos	Serial-number))
)(t	(show-list	\"Old-value\"	(setq Old-Attribute-value (mapcar '(lambda(xx) (strcat (rtos (- N-N (atoi (substr xx 1 (vl-string-search \"、\" xx))))) (substr xx (1+ (vl-string-search \"、\" xx))))) Old-Attribute-value)))
(set_tile	\"Old-value\"	(rtos	Serial-number))
)	)
" )
  (action_tile
    "Move-up"
    "(Record-Dictionaries)
(cond	(	(=	New-0ld	1)
(setq	New-Attribute-value	(list-sort-n-n Serial-number New-Attribute-value)	Serial-number	(if	(= 0 Serial-number) Serial-number	(1- Serial-number)))
(mode-tile-Button \"New-value\" New-Attribute-value Serial-number  \"Move-up\" \"Move-down\" \"Move-Delete\" \"Upside-down\")
)(t	(setq	Old-Attribute-value	(list-sort-n-n Serial-number Old-Attribute-value)
Serial-number	(if	(= 0 Serial-number) Serial-number	(1- Serial-number))
)
(mode-tile-Button \"Old-value\" Old-Attribute-value Serial-number  \"Move-up\" \"Move-down\" \"Move-Delete\" \"Upside-down\")
)	)
" )
  (action_tile
    "Move-down"
    "(Record-Dictionaries)
(setq	Serial-number	(if	(= (1- (length New-Attribute-value)) Serial-number) Serial-number	(1+ Serial-number)))
(cond	(	(=	New-0ld	1)
(setq	New-Attribute-value	(list-sort-n-n Serial-number New-Attribute-value))
(mode-tile-Button \"New-value\" New-Attribute-value Serial-number  \"Move-up\" \"Move-down\" \"Move-Delete\" \"Upside-down\")
)(t	(setq	Old-Attribute-value	(list-sort-n-n Serial-number Old-Attribute-value))
(mode-tile-Button \"Old-value\" Old-Attribute-value Serial-number  \"Move-up\" \"Move-down\" \"Move-Delete\" \"Upside-down\")
)	)
" )
  (action_tile "Move-Delete" "(Move-Delete-Reason)")
  (action_tile
    "Upside-down"
    "(Record-Dictionaries)
(cond	(	(=	New-0ld	1)
(setq	Serial-number	(nth Serial-number New-Attribute-value)	New-Attribute-value	(reverse	New-Attribute-value)	Serial-number	(- (length New-Attribute-value) (length (member Serial-number New-Attribute-value))))
(mode-tile-Button \"New-value\" New-Attribute-value Serial-number  \"Move-up\" \"Move-down\" \"Move-Delete\" \"Upside-down\")
)(t	(setq	Serial-number	(nth Serial-number Old-Attribute-value)	Old-Attribute-value	(reverse	Old-Attribute-value)	Serial-number	(- (length Old-Attribute-value) (length (member Serial-number Old-Attribute-value))))
(mode-tile-Button \"Old-value\" Old-Attribute-value Serial-number  \"Move-up\" \"Move-down\" \"Move-Delete\" \"Upside-down\")
)	)
" )
  (action_tile
    "Upside-bock"
    "(Operation-Dictionaries)
(if	(=	New-0ld	1)
(mode-tile-Button \"New-value\" New-Attribute-value Serial-number  \"Move-up\" \"Move-down\" \"Move-Delete\" \"Upside-down\")
(mode-tile-Button \"Old-value\" Old-Attribute-value Serial-number  \"Move-up\" \"Move-down\" \"Move-Delete\" \"Upside-down\")
)
" )
  (action_tile
    "accept"
    "(setq	Button-1 (get_tile \"Button-1\") Button-2 (get_tile \"Button-2\") Button-3 (get_tile \"Button-3\") Button-4 (get_tile \"Button-4\") Button-5 (get_tile \"Button-5\") Button-6 (get_tile \"Button-6\")
Button-7 (get_tile \"Button-7\") proportion (string->itom-int (get_tile \"proportion\")) Angle-difference (angtof (get_tile \"Angle-difference\")) Angle-Attribute (angtof (get_tile \"Angle-Attribute\"))
)
(done_dialog 1)
" )
  (action_tile
    "help"
    "(vlax-invoke-method (vlax-create-object \"wscript.shell\") 'Popup \"调节新旧图块属性值对应，旧图块被替换后\n会将新图块中属性值改为旧图块的值，即是换\n掉属性块，保持属性值不变；中心点表示图块\n在 WCS下所围成最大矩形的中心；默认以块基\n点插入，若基点不一致，可以按照中心点插入\n乘1倍或者加0度还是等于自身值不变；输入10\n倍数或度数时，可以输入 2*5 或者 20/2或者\n或者11-1又或者 9+1，但是不能输入超过一个\n的四则运算符；若未正常退出则请先同步块。\"  10 \"提示\" 64)"
  )
  (action_tile "cancel" "(done_dialog 0)")
  (setq Serial-number (start_dialog))
  (cond
    ((= 0 Serial-number)
     (if (= Button-7 "1")
       (vlax-ldata-delete "Atblock-Record-Data" "Atblock")
     )
    )
    ((= 1 Serial-number)
     (foreach Obj New-Attribute-value
       (setq New-number-list
	      (append
		New-number-list
		(list
		  (atoi	(substr	obj
				1
				(vl-string-search "、" obj)
			)
		  )
		)
	      )
       )
     )
     (foreach Obj Old-Attribute-value
       (setq Old-number-list
	      (append
		Old-number-list
		(list
		  (atoi	(substr	obj
				1
				(vl-string-search "、" obj)
			)
		  )
		)
	      )
       )
     )
     (setq Serial-number
	    (quantity-Equal New-number-list Old-number-list)
     )
     (if (= Button-7 "1")
       (vlax-ldata-delete "Atblock-Record-Data" "Atblock")
     )
					;(princ New-number-list)(princ Old-number-list)(princ Old-Attribute-value)(princ New-Attribute-value)
     (while (setq n  -1
		  ss (ssget ":S" (list '(0 . "INSERT") (cons 2 Old-blockname)))
	    )
       (cond
	 (ss
	  (setvar "cmdecho" 0)
	  (command "undo" "be")
	  (setq
	    *mSpace* (vla-get-ModelSpace
		       (vla-get-ActiveDocument (vlax-get-acad-object))
		     )
	  )
	  (while (setq n   (1+ n)
		       obj (ssname ss n)
		 )
	    (setq Obj-block	     (vlax-ename->vla-object obj)
		  New-number	     (car Serial-number)
		  Old-number	     (cadr Serial-number)
		  Old-Attribute-list (vlax-safearray->list
				       (vlax-variant-value
					 (vla-getattributes Obj-block)
				       )
				     )
	    )
	    (if	(= "0" button-4)
	      (progn			;(mapcar 'princ (list Button-2 "、" Button-3 "、" proportion "、" Angle-difference))
		(vla-move (setq Now-block (vla-copy New-block))
			  (vla-get-InsertionPoint New-block)
			  (vla-get-InsertionPoint Obj-block)
		)
		(setq New-block	Now-block
		      New-Attribute-list
		       (vlax-safearray->list
			 (vlax-variant-value
			   (vla-getattributes Now-block)
			 )
		       )
		)
		(vla-put-XScaleFactor
		  Now-Block
		  (if (= Button-2 "0")
		    (* proportion (vla-get-XScaleFactor Obj-block))
		    (vla-get-XScaleFactor New-block)
		  )
		)
		(vla-put-YScaleFactor
		  Now-Block
		  (if (= Button-2 "0")
		    (* proportion (vla-get-YScaleFactor Obj-block))
		    (vla-get-YScaleFactor New-block)
		  )
		)
		(vla-put-ZScaleFactor
		  Now-Block
		  (if (= Button-2 "0")
		    (* proportion (vla-get-ZScaleFactor Obj-block))
		    (vla-get-ZScaleFactor New-block)
		  )
		)
		(vla-put-Rotation
		  Now-Block
		  (if (= Button-3 "0")
		    (+ Angle-difference (vla-get-Rotation Obj-block))
		    (vla-get-Rotation New-block)
		  )
		)
		(if (= "1" button-1)
		  (vla-move Now-block
			    (vlax-3d-point
			      (Two-point-midpoint
				(Get-MinMax Now-block)
			      )
			    )
			    (vlax-3d-point
			      (Two-point-midpoint
				(Get-MinMax Obj-block)
			      )
			    )
		  )
		)
		(repeat	(length New-number)
		  (setq	obj1 (nth (1- (car New-number)) New-Attribute-list)
			obj2 (nth (1- (car Old-number)) Old-Attribute-list)
		  )
		  (vla-put-TextString obj1 (vla-get-TextString obj2))
		  (setq Obj1-text (vla-get-TextString obj1))
		  (vla-put-TextString Obj1 "属性块替换工具")
		  (setq	pt1 (vlax-3d-point
			      (Two-point-midpoint (Get-MinMax obj1))
			    )
		  )
		  (if (= Button-5 "1")
		    (vla-put-Rotation obj1 (vla-get-Rotation obj2))
		    (vla-put-Rotation
		      obj1
		      (+ Angle-Attribute (vla-get-Rotation obj2))
		    )
		  )
		  (vla-TransformBy-obj obj1)
		  (vla-move obj1
			    (vlax-3d-point
			      (Two-point-midpoint (Get-MinMax obj1))
			    )
			    pt1
		  )
		  (vla-put-TextString Obj1 Obj1-text)
		  (setq	New-number (cdr New-number)
			Old-number (cdr Old-number)
		  )
		)
		(if (= "1" button-6)
		  (vla-delete Obj-block)
		)
	      )				;end progn
	      (progn
		(setq New-Attribute-list
		       (vlax-safearray->list
			 (vlax-variant-value
			   (vla-getattributes New-block)
			 )
		       )
		)
		(repeat	(length New-number)
		  (vla-put-TextString
		    (nth (1- (car Old-number)) Old-Attribute-list)
		    (vla-get-TextString
		      (nth (1- (car New-number)) New-Attribute-list)
		    )
		  )
		  (setq	New-number (cdr New-number)
			Old-number (cdr Old-number)
		  )
		)
	      )				;end progn
	    )				;end if
	  )				;end while
	  (command "undo" "e")
	  (setvar "cmdecho" 1)
	 )
       )				;end cond
     )					;end while
    )
  )					;end cond
  (princ)
)
(defun Two-point-midpoint
			  (pts / P1 P2 X Y)
  (setq	p1 (car pts)
	p2 (cadr pts)
  )
  (if (= (length p1) (length p2))
    nil
    (setq p1 (list (car p1) (cadr p1))
	  p2 (list (car p2) (cadr p2))
    )
  )
  (mapcar '(lambda (X Y) (/ (+ X Y) 2.0)) P1 P2)
)
(defun Get-MinMax (obj / minext maxext)
  (setq	obj    (if (= (type obj) 'ENAME)
		 (vlax-ename->vla-object obj)
		 obj
	       )
	minext (vlax-make-safearray vlax-vbdouble '(0 . 2))
	maxext (vlax-make-safearray vlax-vbdouble '(0 . 2))
  )
  (vla-getboundingbox obj 'minext 'maxext)
  (list	(vlax-safearray->list minext)
	(vlax-safearray->list maxext)
  )
)
					;此函数在http://bbs.mjtd.com/thread-93123-1-1.html找到的Gu_xl回复
(defun vla-TransformBy-obj (obj / za mat)
  (setq	obj (if	(= (type obj) 'ENAME)
	      (vlax-ename->vla-object obj)
	      obj
	    )
	ZA  (vlax-safearray->list
	      (vlax-variant-value (vla-get-Normal obj))
	    )
	mat (vlax-tmatrix
	      (list
		(list 1 0 (car ZA) 0)
		(list 0 1 (cadr ZA) 0)
		(list 0 0 (caddr ZA) 0)
		(list 0 0 0 1)
	      )
	    )
  )
  (vla-TransformBy obj mat)
)
(prompt "\n属性块替换:ATBLOCK")
(PRINC)