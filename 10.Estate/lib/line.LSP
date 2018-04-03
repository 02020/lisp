
;;程序2-判断两直线是否共线-1
(defun #same_line (ln1 ln2 / pt_start pt_end)
  (setq	ln1	 (vlax-ename->vla-object ln1)
	ln2	 (vlax-ename->vla-object ln2)
	pt_start (vlax-curve-getStartPoint ln1)
	pt_end	 (vlax-curve-getEndPoint ln1)
  )
  (if (and (equal pt_start
		  (vlax-curve-getClosestPointTo ln2 pt_start t)
		  0.001
	   )
	   (equal pt_end
		  (vlax-curve-getClosestPointTo ln2 pt_end t)
		  0.001
	   )
      )
    T
    nil
  )
)


;;程序3-判断两直线是否平行(共线也算平行)
(defun #par_line (ln1 ln2 / pt_start pt_end)
  (setq	pt_start (vlax-curve-getStartPoint ln1)
	pt_end	 (vlax-curve-getEndPoint ln1)
  )
  (if (equal (vlax-curve-getClosestPointTo ln2 pt_end t)
	     (vlax-curve-getClosestPointTo ln2 pt_start t)
	     0.001
      )
    nil
    T
  )
)