(defun e-CreateToolbar (Openlx StrToolsName lst / file str1 str2 strl n1 name sym bmp strzh)
  (setq oINT (+ oINT 100))
  (setvar "cmdecho" 0)
  (vl-load-com)
  (setq file (open "D:\\1_080\\Soft\\080_CAD\\程序\\080工具栏.mnu" Openlx))
  (setq str1 "***MENUGROUP=080工具栏\n\n***TOOLBARS"
        str2 (strcat "ID_TEMP_"
                     StrToolsName
                     "000"
                     "   [_Toolbar(\""
                     StrToolsName
                     "\", _Floating, _Hide, 50, "
                     (rtos oINT 2 0)
                     ", 1)]"
             )
  ;str2 (strcat "ID_TEMP_" StrToolsName "000" "   [_Toolbar(\"" StrToolsName "\", _bottom, _Show, 1, 0, 1)]")
  ;[_Toolbar("文修简化工具栏", _Top, _Show, 1, 0, 1)]_top 
  )

  (if (= Openlx "w")
    (princ (strcat str1 "\n") file)
    (princ "\n" file)
  ) ;打印到文件
  (princ (strcat "\n**" StrToolsName "\n") file)
  (princ (strcat str2 "\n") file) ;打印到文件
  (setq n1 -1)
  (while (setq strl (nth (setq n1 (+ n1 1)) lst))
    (setq name (nth 0 strl)
          sym  (nth 1 strl)
          bmp  (nth 2 strl)
    )
    (setq INTno (rtos (+ n1 1) 2 0)
          INTno (substr (strcat "00" INTno) (strlen INTno))
    )
    (setq strzh (strcat "ID_TEMP_" StrToolsName INTno "   [_Button(\"" name "\", \"" bmp "\", \"" bmp "\")]" sym)
    )
    (princ (strcat strzh "\n") file)
  )
  (close file)
  (princ)
)







;|
;;; ================================================================
;;; 功能：自动为.isp命令创建一个命令的按钮子函数
;;; 使用: 在.isp文件最后最下方写入一句 (createtoolbar name sym bmp )
;;;       其中:name为命令中文名称,sym为执行的命令,bmp为图标,最好采用
;;;       系统图标,自定义图标需拷贝到支持目录,为*.bmp格式.
;;; 样例:(createtoolbar "轴测图直线" "zline" "rcdata_16_osnend")
;;;      (createtoolbar "轴测图直线" "zline" "zline.bmp")
;;; 作者：langjs                                   日期:2012年4月1日
;;; ================================================================
	;(createtoolbar "轴测图直线" "zline" "RCDATA_16_OSNEND")
	
(defun createtoolbar (name sym bmp / file x)
  (setvar "cmdecho" 0)
  (vl-load-com)
  (if (menugroup "080工具栏") (command "menuunload" "080工具栏"))
  (alert "\n程序将自动创建\"临时工具栏\"，请稍后")
  (setq file (open "080工具栏.mnu" "W"))
  (foreach x (list "***MENUGROUP=080工具栏\n\n***TOOLBARS\n**080工具栏"
     "ID_TEMP001   [_Toolbar(\"临时工具栏\", _Floating, _Hide, 100, 340, 1)]" 
    (strcat "ID_TEMP011   [_Button(\"" name "\", \"" bmp "\", \"" bmp "\")]^C^C" sym)
      )
    (princ (strcat x "\n") file)
  )
  (close file)
  (command "menuload" "080工具栏.mnu")
  (command "TOOLBAR" "临时工具栏" "S")
  (princ)
)
;(createtoolbar "轴测图直线" "zline" "RCDATA_16_OSNEND")
(defun c:111 ()
  (createtoolbar "厨" "TWC" "RCDATA_16_OSNEND")

  )

|;

  





(defun e-CreateMenu (menulist / acadapp CMD currmenugroup doc loadedmenulist menu newmenu)
  (vl-load-com)
  (setq acadapp        (vlax-get-acad-object)
        loadedmenulist '()
        doc            (vla-get-activedocument acadapp)
        currmenugroup  (vla-item 	 (vla-get-menugroups
                                   (vla-get-application doc)
                                 )
                                 "ACAD"
				 )
	CMD            (lambda (x)
			 (strcat "\033\033\137" x "\040")
			 )
	)
  (if (/= currmenugroup nil)
    (progn
      (vlax-for menu (vla-get-menus currmenugroup)
        (setq loadedmenulist
               (cons
                 (vla-get-namenomnemonic menu)
                 loadedmenulist
               )
        )
      )
      (if (= (vl-position (car menulist) loadedmenulist) nil)
        (progn
          (setq newmenu (vla-add (vla-get-menus currmenugroup)
                                 (car menulist)
                        )
          )
          (foreach e (car (cdr menulist))
            (vla-addmenuitem
              newmenu
              (1+ (vla-get-count newmenu))
              (car e)
              (CMD (cdr e))
            )
          )
          (vla-insertinmenubar
            newmenu
            (1+ (vla-get-count
                  (vla-get-menubar acadapp)
                )
            )
          )
        )
;;;	(princ (strcat "\nerror of type ohno "
;;;                       (car menulist)
;;;                       " menu is already loaded "
;;;		       )
;;;	       )
	(princ (strcat (car menulist) " menu is already loaded "))
      )
    )
  )
  (princ))
 