;;;-===-==-==-==-=-==-==-==-=-==-==-=-==-==-=-==-==-=-==-==-=-==-=-==-
;;; (crpd:loadDynamicPopUpMenu '("Mymenu"(("Do Command1" . "command1")("Do Command2" . "command2"))))
;;;-===-==-==-==-=-==-==-==-=-==-==-=-==-==-=-==-==-=-==-==-=-==-=-==-

(defun c:vfcd()
  (#crpd:loadDynamicPopUpMenu
    '("BMF工具" (
		 ("老数据..." . "SJBRCHXX")
		 ("检查权属关系...". "VFQSGX")
		 ("下载分户图..." . "XZFHT")		
		 ("批量修改户室属性...". "PLXGHSSX")
		 ("系统图层切换...". "XSXTTC")
		)
      )
    )
  (PRINC))










(defun #crpd:loaddynamicpopupmenu (menulist / acadapp CMD currmenugroup doc loadedmenulist menu newmenu)
  (vl-load-com)
  (setq acadapp        (vlax-get-acad-object)
        loadedmenulist '()
        doc            (vla-get-activedocument acadapp)
        currmenugroup  (vla-item (vla-get-menugroups
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
 