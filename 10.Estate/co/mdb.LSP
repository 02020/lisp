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

(setq dbfile (findfile "D:\\CAD.mdb")) ;�������ݿ����Ƽ�·��
(Setq conn(vlax-create-object "ADODB.Connection")) ;����ADO�ؼ�
(setq connstring (strcat "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" dbfile)) ;�������ݿ������ַ���
(vlax-invoke-method conn "open" connstring "" "" -1) ;�����ݿ�����

 (setq data (ADO_DoSQL conn "select * from [ͼ��ת��]"))
  (princ data)
  )