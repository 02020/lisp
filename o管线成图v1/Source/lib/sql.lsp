
;;构建更新语句  dataList '(("Title" . "Floorplan") ("Project" . "Project A"))) 
(defun BulidUpdateSql (conn tableName dataList key / strSql)
  (setq fieldList (ADO_GetFieldNames conn tableName)
fieldList     (mapcar '(lambda (x)  (cons (car x) (cadr x))) fieldList)

	)
  (setq strSql "")
  (setq val (cdr (assoc key dataList)))
  (foreach item	dataList
    (setq field (car item))
    (if	(/= (car item) key)
      (progn
	(setq value	(cdr item)
	      fieldType	(cdr (assoc field fieldList))
	)

	;;如果是整数
	(if (and
	      (= value "")
	      (or (= fieldType "Real")
		  (= fieldType "Integer")
	      )
	    )
	  (setq value "0")
	)
	(setq strSql (strcat strSql "[" (car item) "]='" value "',"))

      )
    )
  ) ;end-foreach

  (setq
    strSql (substr strSql 1 (- (strlen strSql) 1))
    strSql (strcat "update [" tableName "] set " strSql " where " key "='" val "'")
  )
  strSql
)




























































(defun BulidSql	(fields tableName /)
  (setq sql "")
  (foreach field fields
    (setq sql (strcat sql "[" (cadr field) "],"))
  )
  (setq
	sql (substr sql 1 (- (strlen sql) 1))
	sql (strcat "select " sql  " from [" tableName "] where 1=1 ")
		    
  )
  sql
)
