(progn
  (vl-load-com)
  (vl-load-reactors)
  (setvar "cmdecho" 0)
)


;;;
;;; (setq ADOLISP_ADODLLPath
;;;            (findfile "c:\\program files\\common files\\system\\ado\\msado15.dll"))
;;;
;;;(vlax-import-type-library
;;;       :tlb-filename ADOLISP_ADODLLPath :methods-prefix
;;;       "ADOMethod-" :properties-prefix "ADOProperty-"
;;;       :constants-prefix "ADOConstant-"
;;;     )



(defun c:1()

(setq dbfile (findfile "D:\\CAD.mdb")) ;设置数据库名称及路径
(Setq conn(vlax-create-object "ADODB.Connection")) ;引用ADO控件
(setq connstring (strcat "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" dbfile)) ;设置数据库连接字符串
(vlax-invoke-method conn "open" connstring "" "" -1) ;打开数据库连接

 (setq data (ADO_DoSQL conn "select * from [图层转换]"))
  (princ data)
  )
