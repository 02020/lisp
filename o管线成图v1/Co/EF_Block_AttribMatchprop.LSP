;取得所有属性表
(defun EF_Block:GetAllAttrib (ss / i n lstAttrib ename)
  (setq i -1 n (sslength ss))
  (while (< (setq i (1+ i)) n)
    (setq ename (ssname ss i))
    (setq lstAttrib (append lstAttrib (EF:Block-GetAttrib ename)))
    )
  lstAttrib
  )

;属性匹配
(defun C:EF_Block_AttribMATCHPROP ( / ss1 ss2 lstAttrib)
  (prompt "\n选择源属性块:")
  (if (and (setq ss1 (ssget ":S" '((0 . "INSERT") (66 . 1))))
	   (progn
	     (prompt "\n选择目标属性块:")
	     (setq ss2 (ssget '((0 . "INSERT") (66 . 1))))
	     )
	   )
    (progn
      (setq lstAttrib (EF_Block:GetAllAttrib ss1))
      (setq lstAttrib2 (EF_Block:GetAllAttrib ss2))
      (mapcar '(lambda (e / ename edata att)
		 (setq ename (last e))
		 (if (setq att (assoc (car e) lstAttrib))
		   (progn
		     (setq edata (entget ename))
		     (setq edata (subst (cons 1 (cadr att)) (assoc 1 edata) edata))
		     (entmod edata)
		     )
		   )
		 )
	      lstAttrib2
	      )
      )
    )
  lstAttrib
  )

;属性改变
(defun C:EF_Block_AttribCopy ( / ss1 ss2 lstAttrib lstAttrib2)
  (prompt "\n选择源属性块:")
  (if (and (setq ss1 (ssget '((0 . "INSERT") (66 . 1))))
	   (progn
	     (prompt "\n选择目标属性块:")
	     (setq ss2 (ssget '((0 . "INSERT") (66 . 1))))
	     )
	   )
    (progn
      (setq lstAttrib (EF_Block:GetAllAttrib ss1))
      (setq lstAttrib (mapcar '(lambda (e)
				(list (car e)
				      (cadr e)
				      (cdr (assoc 10 (entget (last e))))
				      )
				)
			      lstAttrib
			      ))
      (setq lstAttrib2 (EF_Block:GetAllAttrib ss2))
      (mapcar '(lambda (e / ename edata att lstAtt)
		 (setq ename (last e))
		 (setq edata (entget ename))
		 (setq pt10 (cdr (assoc 10 edata)))
		 (setq lstAtt (mapcar '(lambda (e)
					 (list (distance pt10 (last e))
					       (car e)
					       (cadr e)
					       )
					 )
				      lstAttrib
				      ))
		 (setq lstAtt (vl-sort lstAtt
				       '(lambda (e1 e2)
					  (< (car e1) (car e2))
					  )
				       )
		       )
		 (setq att (car lstAtt))
		 (setq edata (entget ename))
		 (setq edata (subst (cons 1 (caddr att)) (assoc 1 edata) edata))
		 (setq edata (subst (cons 2 (cadr att)) (assoc 2 edata) edata))
		 (entmod edata)
		 )
	      lstAttrib2
	      )
      )
    )
  lstAttrib
  )

