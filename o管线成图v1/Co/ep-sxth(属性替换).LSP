;; 需要e派工具箱(XCAD)的支持：http://yunpan.cn/QXQKsW9gAPmpF 访问密码 2ffa
;; 2014年05月23日　ep-sxth(属性替换)
(defun c:ep-sxth (/ ilst ll1 ll2)
  (xyp-cmdla0)
  (defun main-pro (/ ss i s1 a)
    (setq i -1
	  num 0
    )
    (princ "\n选择属性块: ")
    (if	(setq ss (ssget '((0 . "insert") (66 . 1))))
      (while (setq s1 (ssname ss (setq i (1+ i))))
	(foreach a (xyp-Att2list s1)
	  (if (= (caddr a) tx-old)
	    (progn
	      (xyp-SubUpd (car a) 1 tx-new)
	      (setq num (1+ num))
	    )
	  )
	)
      )
    )
    (if	(> num 0)
      (alert (strcat "共修改 " (itoa num) " 个属性块"))
    )
  )
  (xyp-initSet '(tx-old tx-new) '("ES-001" "香樟"))
  (setq	ilst '(("" "参数" ":boxed_column{")
	       ("tx-old" "原值" "str" "12")
	       ("tx-new" "新值" "str" "12")
	       "spacer;"
	       "}"
	       "spacer;"
	       "ioc"
	      )
  )
  (if (= (xyp-Dcl-Init Ilst "【属性替换】" t) 1)
    (main-pro)
  )
  (xyp-cmdla1)
)