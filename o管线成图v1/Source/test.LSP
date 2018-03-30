

;;;(init)

(defun c:2 ()
  (SetCurrentLayer)
)



(defun c:4 ()
  (EraseRepeatData)
)




(defun c:3 ()
  (setq dcl_id (load_dialog "scroll-bar"))
  (new_dialog "scrolling" dcl_id)

  (setq	n   0
	num 100000
  )

  (repeat num
    (setq n (1+ n))
    (setq schedule (* 2.0 (/ n 2.0 num)))
    (scroll-bar schedule)
  )
  (done_dialog)
  (unload_dialog dcl_id)
)


(defun c:3 ()
  (setvar "osmode" 0)
  (setq blockSize "0.5")
  (setq dcl_id (load_dialog "scroll-bar"))
  (new_dialog "scrolling" dcl_id)

  (setq filePath "D:\\C3.CAD\\03.Code\\o管线成图v1\\Data\\成果1\\丽日花苑原始坐标数据.txt")
  (setq	lst	 (ReadTxt filePath)
	num	 (length lst)
	n	 0
	layerNot nil
  )
  (foreach line	lst
    (setq n (1+ n))
    (setq schedule (* 2.0 (/ n 2.0 num)))
    (scroll-bar schedule)
  )
  (done_dialog)
  (unload_dialog dcl_id)
)




(defun c:Example_Erase ()
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))

  ;; Create the new selection set
  (setq ssetObj (vla-Add (vla-get-SelectionSets doc) "EraseRepeatData"))


  (vla-ZoomAll acadObj)

  (setq ssobjs (vlax-make-safearray vlax-vbObject (cons 0 (- (vla-get-Count modelSpace) 1))))
  (setq I 0)
  (while (>= (- (vla-get-Count modelSpace) 1) I)
    (vlax-safearray-put-element ssobjs I (vla-Item modelSpace I))
    (setq I (1+ I))
  )

  ;; Add the array of objects to the selection set
  (vla-AddItems ssetObj ssobjs)

  ;; List all the objects in the selection set
  (if (= (vla-get-Count ssetObj) 0)
    (alert "The selection set is empty")
    (progn
      (setq I 0)
      (while (>= (- (vla-get-Count ssetObj) 1) I)
	(alert (strcat "The selection set contains: " (vla-get-ObjectName (vla-Item ssetObj I))))
	(setq I (1+ I))
      )
    )
  )

  ;; Erase the objects in the selection set
  (vla-Erase ssetObj)

  ;; List all the objects in the selection set
  (if (= (vla-get-Count ssetObj) 0)
    (alert "The selection set is empty")
    (progn
      (setq I 0)
      (while (>= (- (vla-get-Count ssetObj) 1) I)
	(alert (strcat "The selection set contains: " (vla-get-ObjectName (vla-Item ssetObj I))))
      )
    )
  )
)








