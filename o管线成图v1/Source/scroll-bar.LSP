(vl-load-com)
(setq *AcadApp*
       (vlax-get-acad-object)
) ;_ end of setq
(setq *ActiveDocument*
       (vla-get-ActiveDocument
         *AcadApp*
       ) ;_ end of vla-get-ActiveDocument
) ;_ end of setq

(setq *Linetypes*
       (vla-get-Linetypes
         *ActiveDocument*
       ) ;_ end of vla-get-Linetypes
) ;_ end of setq

;;; ;加载线型的函数
(defun LoadLinetype (LinetypesName
		     /
		     Found
		     Linetypeobj
		    )
  (setq Found 0)
  (vlax-for Linetypeobj	*Linetypes*
    (if
      (equal
	(vla-get-Name Linetypeobj)
	LinetypesName
      ) ;_ end of equal
       (setq Found 1)
    ) ;_ end of if
  ) ;_ end of vlax-for
  (if (= 0 Found)
    (vla-Load *Linetypes*
	      LinetypesName
	      "acadiso.lin"
    ) ;_ end of vla-Load
  ) ;_ end of if

) ;_ end of defun
;;ss选择集  功能:实线转成虚线，除实线外的线转成实线
(defun convert-linetype (ss
		     /
		     dcl_id v lst num sslst schedule v
		    )

  (LoadLinetype "DASHED")
  (setq dcl_id (load_dialog "scroll-bar"))
  (new_dialog "scrolling" dcl_id)
  (setq	num   (sslength ss)
	n     0
	sslst (ss->enlist ss)
  )

  (foreach v sslst
    (progn (edit_ent_lineType v)
	   (set_tile "txt" (strcat "正在处理第：" (rtos n) "实体"))
	   (setq schedule (* 2.0 (/ n 2.0 num)))
	   (scroll-bar schedule)
	   (setq n (1+ n))
    )
  )
  (done_dialog)
  (unload_dialog dcl_id)

)
;;;修改线型函数
(defun edit_ent_lineType (ent / vlObj lineType)
  (setq	vlObj	 (vlax-ename->vla-object ent)
	lineType (vla-get-linetype vlObj)
  )
  (if (= (strcase lineType) "CONTINUOUS")
    (vla-put-linetype vlObj "DASHED")
    (vla-put-linetype vlObj "Continuous")
  )
  (vla-Update vlObj)

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
(defun c:test (/ ss)
  (setq ss (ssget))
  (convert-linetype ss)
)