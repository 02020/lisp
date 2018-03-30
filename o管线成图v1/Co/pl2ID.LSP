(defun c:11()
(setq ssPl (ssget))
  (setq n1 -1)
  (while (setq ssna (ssname ssPl (setq n1 (1+ n1)) ))
    (setq lstss (cons ssna lstss))
    )
(PlCo2PlColst lstss)
  )
	; (setq lstss (cdr (reverse (ssnameX ssPl))))





(defun PlCo2PlColst (PlCo / plObj PlCoN lstPtN en PlCo PlCoN lstN pln re)
  ;;提取多段线对象为表 (元素 起点 终点)	
  (setq	PlCoN  nil
	en     (car PlCo)		;提取第一个元素
	PlCo   (cdr PlCo)		;减去第一个元素
	PlCoN  (cons en PlCoN)
	lstPtN nil
  )
  (setq	pln (length Plco)
	re  0
  )
  ;;用来控制循环
  (while (and Plco (>= pln re))
    (setq re (1+ re))
    ;;遍历所有新点
    (foreach ptt lstPtN
      ;;遍历PlCo表的元素
      (foreach en PlCo
	(if (member ptt en)
	  (progn
	    (setq PlCo (vl-remove en PlCo))
	    ;;如果ptt不是多段线的起点则倒置多段线
	    (if	(not (equal ptt (cadr en) 0.0001))
	      (setq en (cons (car en) (reverse (cdr en))))
	    )
	    (setq EnN nil)
	    (foreach x PlCoN
	      (if (member ptt x)
		(setq EnN x)
	      )
	    )
	    (setq PlCoN (insertEn (vl-position EnN PlCoN) en PlCoN))
	    (setq lstPtN (insertPt (vl-position ptt lstPtN)
				   (cddr en)
				   lstPtN
			 )
	    )
	  )
	)
	;;end if
      )
    )
  )
  (if (null PlCo)
    (progn
      (setq lstN nil)
      (mapcar '(lambda (x)
		 (if (not (member x lstN))
		   (setq lstN (cons x lstN))
		 )
	       )
	      lstPtN
      )

      (reverse lstN)
    )
    (PlCo2PlColst PlCo lstPtN)

  )
)
