;; 图元en  图元信息ent

  ;判断多段线内是否存在多段线
(defun e-GetPlinesArea (en / ent layerName area enIn plinePoints)
  (setq ent         (entget en)
        layerName   (cdr (assoc 8 ent))
        plinePoints (e-GetPlinePoints en)
        area        (e-GetPlineArea en)
  )
  ;;判断是否内部是否存在多段线，例如：天井
  (setq ssIn (ssget "WP" plinePoints (list (cons 0 "LWPOLYLINE") (cons 8 layerName))))
  (if (not (null ssIn))
    (progn (setq areaIn 0
                 n1 -1
           )
           (while (setq enIn (ssname ssIn (setq n1 (1+ n1))))
             (setq areaIn (+ areaIn (e-GetPlineArea enIn)))
           )
           (setq area (- area areaIn))
    )
  )
  area
)


(defun c:1()
    (setq ss (ssget (list (cons 0 "LWPOLYLINE") )))
  (princ (e-GetPlinesArea (ssname ss 0)))
  (princ)
  )







 ;;获取多段线面积
(defun e-GetPlineArea (en / area ent xx)
  (setq area (vla-get-area (vlax-ename->vla-object en))
        ent  (entget en)
        xx   (cdr (assoc 6 ent))
  )
  (if (or (= xx "HIDDENX2") (= xx "HIDDENX2") (= xx "DASHED"))
    (setq area (/ area 2))
  )
  area
)



;;获取两直线的交点坐标
(defun e-GetInters (OBJ1 OBJ2 / PS LST)
    (setq PS (vla-intersectwith OBJ1 OBJ2 3)
	  PS (vl-catch-all-apply
	       'vlax-safearray->list
	       (list (vlax-variant-value PS))
	     )
    )
    (if	(and PS (not (vl-catch-all-error-p PS)))
      (while (setq LST (cons (list (car PS) (cadr PS)) LST)
		   PS  (cdddr PS)
	     )
      )
    )
    LST
  )


;;获取中点
(defun e-GetMid (pt1 pt2 / x)
  (mapcar '(lambda (x) (/ x 2)) (mapcar '+ pt1 pt2))
)











;;;┏━━━━━━━━━━━━━━━━━━┓
;;;┃の 获取多段线内文字                 ┃
;;;┗━━━━━━━━━━━━━━━━━━┛
(defun e-GetPlineText
       (ddx DYlayer / pllst pllstTC dmss dm ptd pt dmsel cdy)
  (setq pllst   (e-GetPlinePoints ddx)
        bound   (e-GetBound ddx)
        cdy     (abs (- (cadr (car bound)) (cadr (cadr bound))))
     
        ptd     (e-GetMid pllst)
  )
  ptd 
  (strcat (rtos (car ptd) 2 2) "," (rtos (cadr ptd) 2 2))
  (setq dmss (ssget "WP" pllst (list (cons 0 "TEXT") (cons 8 DYlayer))))
  (if (not (null dmss))
    (progn
      (cond
        ((= (sslength dmss) 1)
         (progn
           (setq ent (entget (ssname dmss 0))
             dm (cdr (assoc  1 ent ))
             pt (cdr (assoc  10 ent ))
                 )
           (princ (strcat "<" dm ">"))
         )
        )
        (t ;(> (sslength dmss) 1)
         (progn
           (setq dm "")
         )
        )
      ) ;end-cond
    )
  )
  (if (or (null dm) (< (strlen dm) 1))
    (progn
      (command "UNDO" "m")
      (command "zoom" "c" ptd (* cdy 2))
(e-MarkPline ddx)
      (setq dm (getstring "输入文本:"))
      (if (< (strlen dm) 1)
        (progn
          (while (null (setq dmsel (entsel "\n请选择多段线所属文本"))))
          (setq dm (cdr (assoc 1 (entget (car dmsel)))))
          (setq pt (cdr (assoc 10 (entget (car dmsel)))))
        )
        (setq pt '(0 0))
      )
      (command "UNDO" "B")
    )
  )
  (list dm pt ptd)
)

(defun e-MarkPline(pl / plinePointsRandom)
  (setq   plinePointsRandom (e-GetPlinePointsRandom pl))
        (grvecs
        (append
          '(2)
          plinePointsRandom
        )
      )
  )

(defun e-GetPlinePoints (ddx / x)
  (vl-remove-if 'not (mapcar '(lambda (x) (if (= (car x) 10) (cdr x)) ) (entget ddx)) )
  )



(defun e-GetMid (plist)
  (mapcar '(lambda (x) (/ x 2))
          (mapcar '+
                  (apply 'mapcar (cons 'min plist))
                  (apply 'mapcar (cons 'max plist))
          )
  )

)


(defun e-GetExtents (plist /)
    (list
        (apply 'mapcar (cons 'min plist))
        (apply 'mapcar (cons 'max plist))
    )
)


  ;取得图元的外形最小点与最大点
(defun e-GetBound (ent / obj pta ptb)
  (setq obj (vlax-ename->vla-object ent))
  (vla-GetBoundingBox obj 'pta 'ptb)
  (list (vlax-safearray->list pta) (vlax-safearray->list ptb))
)


  ;是随机数
(defun e-GetPlinePointsRandom (ddx / Obj start ptend i param plst pt)
  (setq plObj (vlax-ename->vla-object ddx)
        start (vlax-curve-getStartParam plObj)
        ptend (vlax-curve-getEndParam plObj)
        i     -1
  )
  (setq csz (* (- ptend start) 8))
  (while (< (setq i (1+ i)) csz)
    (setq param (* i (/ (- ptend start) csz))
          pt    (vlax-curve-getPointAtParam plObj param)
          plst  (append plst (list pt))
    )
  )
  (vl-sort plst (function (lambda (e1 e2) (> (cadr e1) (cadr e2)))))
)