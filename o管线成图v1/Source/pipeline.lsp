;; ����޸�ʱ�� 2018��4��10��


 ;| ����������:GGT
__ 0.�������ݿ�
__ 1.������������ݣ�mdb��

__ 3.�������� ---- �����������㣬�ظ���
__ 4.����ͼ������鷽��
__ 5.

__ ��ע:���ǵ� lstpt ����Ϊ��ά�㣬�̵߳�Ϊ��ά����Ҫת��


__ 2.���� ʸ����ͷ ���� �����

__ 4.���� �����,��һ��˳���ʾ������
|;



(defun c:vfcd()
 ; (vla-removefrommenubar "BMF����")
  (e-CreateMenu
    '("BMF����" (
	
		 ("ϵͳͼ���л�...". "XSXTTC")
		)
      )
    )
  (PRINC))


(defun c:d ()
(setq menu (vlex-MenuGroups))
  (princ menu)
  (princ)


  
;(FetchPointIntoDwg)
)


(defun SetDataMdb ()
  (setq dbfile (e-GetFiles "ѡ��mdb" "mdb|*.mdb*"))
  (if (= dbfile "")
    (princ "\no__o �����˳�")
    (e-regset "dataMdb" dbfile)
  )
  (princ)
)



(defun c:2()
    (SetDataMdb)
  )


;| 1.������������� |;
(defun FetchPointIntoDwg (/ bmList layerNot layerName schedule block filePath)
  (setvar "osmode" 0)
  (setq blockSize "0.5")
  (setq dbfile (e-regget "dataMdb"))
  (Setq conn (vlax-create-object "ADODB.Connection")) ;����ADO�ؼ�
  (setq connstring (strcat "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" dbfile)) ;�������ݿ������ַ���
  (vlax-invoke-method conn "open" connstring "" "" -1) ;�����ݿ�����

  (setq dcl_id (load_dialog "scroll-bar"))
  (new_dialog "scrolling" dcl_id)

  (setq	sql	  (BulidSelectSql configPoint "���")
	data	  (ADO_DoSQL conn sql)
	n	  -1
	i	  0
	num	  (length data)
	layerNot  nil
	blocksNot nil
  )

 ;�ų�����
  (while (setq item (nth (setq i (1+ i)) data))
    ;;������
    (setq schedule (* 2.0 (/ n 2.0 num)))
    (scroll-bar schedule)
    (setq x    (nth 0 item)
	  y    (nth 1 item)
	  h    (nth 5 item)
	  cat  (nth 2 item)
	  code (nth 3 item)
	  node (nth 4 item) ;�ڵ�����
    )

    ;;�����
    (if	(and (not (null x)) (not (null y)) (not (null h)))
	(InserPipelineBlock item)
    )
  )
  (setq layStr (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) layerNot)))
  (princ (strcat "\no__o ���±��벻���ڶ�Ӧͼ�� " layStr))
  (princ "\no__o ����˶Խڵ����ڵ�ͼ��,�ֶ����е��� ")
  ;;ж�ؽ�����
  (done_dialog)
  (unload_dialog dcl_id)
  (ADO_DisconnectFromDB conn)
  (princ)
)

;;������߿�
(defun InserPipelineBlock (item)
  (setq	x	  (nth 0 item)
	y	  (nth 1 item)
	h	  (nth 5 item)
	cat	  (nth 2 item)
	code	  (nth 3 item)
	node	  (nth 4 item) ;�ڵ�����
	pt	  (list (distof x 2) (distof y 2))
	layerName (GetLayerFormNode cat "POINT")
  )

  (if (/= (TBLSEARCH "BLOCK" node) nil)
    (progn
      (setq block (vla-insertblock mSpace (vlax-3d-point pt) node blockSize blockSize blockSize 0))
      (SetPipelineBlockXDatas block configPoint item)
      (if layerName
	(vla-put-layer block layerName)
	(if (not (member cat layerNot))
	  (setq layerNot (cons cat layerNot))
	)
      )
    )
    (progn
      (if (not (member node blocksNot))
	(setq blocksNot (cons node blocksNot))
      )
    )
  )
)

;;���ӿ��xData����ֵ
(defun SetPipelineBlockXDatas(o fields item)
  (defun XData (o key value)
    (ex:PutXData o (list (cons 1001 key) (cons 1000 value)))
  )
  (foreach field fields
   (setq value (nth (- (car field) 1) item ))
   (XData o (cadr field) value)
  )
)

;;��ȡxData����ֵ
(defun GetXDatas (en keyList dem)
  (setq	o	(vlax-ename->vla-object en)
	keyList	(mapcar '(lambda (x) (cadr x)) keyList)
  )
  (if (assoc -3 (entget en keyList))
    (apply
      'strcat
      (mapcar '(lambda (x) (strcat (GetXData o x) dem)) keyList)
    )
    nil
  )
)

;;��ȡxData����ֵ
(defun GetXDataMap (en keyList)
  (setq	o	(vlax-ename->vla-object en)
	keyList	(mapcar '(lambda (x) (cadr x)) keyList)
  )
  (if (assoc -3 (entget en keyList))

      (mapcar '(lambda (x) (list x (GetXData o x) )) keyList)
    
    nil
  )
)



;;
(defun GetXData	(vlaObj key)
  (cdr (assoc 1000 (ex:GetXData vlaObj key)))
)







;| ��ʾ�ܾ����� |;
(defun c:s ()
  (setq fontSize "0.5")
  (setvar "pickbox" 20)
  (while (setq en (entsel "\no__o ��ѡѡ�� ��ͷ"))
    (setq text (GetXDatas (car en) configPoint ","))
    (if	text
      (princ (strcat "\no__o " text))
      (princ "\no__o �ü�ͷδ��������")
    )
  )
  (setvar "pickbox" 10)
)



;| �޸�����ֵmdb |;
(defun UpdatePointData ()
  (princ (strcat "\no__o " "�޸�����ֵ"))
  (setq dbfile "D:\\C3.CAD\\03.Code\\o���߳�ͼv1\\Data\\�ɹ�1\\���ջ�԰���߿�.mdb")
  (Setq conn (vlax-create-object "ADODB.Connection")) ;����ADO�ؼ�
  (setq connstring (strcat "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" dbfile)) ;�������ݿ������ַ���
  (vlax-invoke-method conn "open" connstring "" "" -1) ;�����ݿ�����
  (setq tableName "���")
  (setvar "pickbox" 20)
  (while (setq en (entsel "\no__o ��ѡѡ�� ��ͷ"))
    (setq x	 (GetXDataMap (car en) configPoint)
	  x	 (dos_proplist "�ڵ�����" "�޸�����" x)
	  code   (cdr (assoc "��ҵ���" x))
	  strSql (BulidUpdateSql conn tableName x "��ҵ���")
    )
    (if (vl-symbol-value   	(ADO_DoSQL conn strSql))
	(princ (strcat "\no__o " code " �޸ĳɹ�"))
    )
  )
  (setvar "pickbox" 10)
    (ADO_DisconnectFromDB conn)
  (princ)
)





(defun c:ss()
(UpdatePointData)
  )



;;;
;;;
;;;
;;;(setq x 
;;;
;;;(("Title" . "Floorplan") ("Project" . "Project A")) 
;;;
;;; 
;;;
;;;Command: 
;;;
;;;(("Title" . "Floorplan") ("Project" . "Project B")) 
;;;
;;; 
;;;
;;;



;;���ݽڵ��ȡͼ��
(defun GetLayerFormNode	(bm lx / layer layerName layerColor)
 ;; (defun *error* (msg)(princ (strcat "\no__o GetLayerFormNode:" msg)))
  ;configLayerList configNodeList
  (setq 
    LayerSel (vla-get-Layers AcadDocument)
    layer    (assoc bm configLayerList)
  )
  (if (null layer)
    (progn nil)
    (progn
      (setq layerName  (strcat (cadr layer) lx)
	    layerColor (caddr layer)
      )
      (if (not (tblsearch "layer" layerName))
	(vla-put-Color (vla-add LayerSel layerName) layerColor)
      )
      layerName
    )
  )
)


;;ɾ���ظ�����
(defun EraseRepeatData (/ ss ss2 c ent att)
  (princ "\no__o ɾ���ظ�����")
  (setq blockName "0-Elevation")
  (if (setq ss (ssget (list (cons 0 "INSERT") (cons 2 blockName))))
    (progn
      (setq c 0
	    ptlist nil
      )
      (repeat (sslength ss)
	(setq
	 current (ssname ss c)
	  ent (vlax-ename->vla-object current)
	      pt  (cdr (assoc 10 (entget current)))
	)
	(if  (member pt ptlist)
	  (progn
	    (vla-highlight ent :vlax-true)
	    (if	(not ss2)
	      (setq ss2 (ssadd current))
	      (ssadd current ss2)
	    )
	  )
	   (setq ptlist (cons pt ptlist))
	)
	(setq c (1+ c))
      )
    )
  )
  (if ss2
    (progn
    (command "erase" ss2 "")
    
    
  (princ (strcat "\no__o �ڵ��������" (itoa (sslength ss)) "������ɾ���ظ��ĸ���Ϊ��" (itoa (sslength ss2))))
    )
    )
)









 ;| ����ɹ����� |;
(defun ExportDataNodeArrow (/ ssPl ssoh lstssPl lstssOh lstOh lstPl)
  (defun Cad2txt_Arrow (lst Path /)
    (if	(and (setq File Path) (setq F2 (open File "w")))
      (progn
	(princ "���|���|�յ�|���|����|����|�ܾ�|�׸���|���跽ʽ" F2)
	(setq n2 -1)
	(while (setq pt (nth (setq n2 (1+ n2)) lst))
	  (setq	pts  (nth 0 pt)
		pte  (nth 1 pt)
		data (nth 2 pt)
	  )
	  (princ
	    (strcat "\n"
		    (itoa (1+ n2))
		    "|"
		    (rtos (cadr pts))
		    ","
		    (rtos (car pts))
		    "|"
		    (rtos (cadr pte))
		    ","
		    (rtos (car pte))
		    "|"
		    data

	    )
	    F2
	  )
	)
	(close F2)
      )
      (princ "\no__o �޷����")
    )
  )
  (defun Cad2txt_oh (lst Path /)
    (if	(and (setq File Path) (setq F2 (open File "w")))
      (progn
	(princ "���|���|�߳�|�߲�|����|�ڵ�����" F2)
	(setq n2 -1)
	(while (setq lstoh (nth (setq n2 (1+ n2)) lst))
	  (setq pt (cdr (assoc 10 lstoh)))
	  (setq ptstr (strcat (rtos (cadr pt)) "," (rtos (car pt))))
	  (princ (strcat "\n"
			 (itoa (1+ n2))
		    	"|"
			 (cdr (assoc "id" lstoh))
			 "|"
			 (cdr (assoc "h" lstoh))
			 "|"
			 (cdr (assoc "hc" lstoh))
			 "|"
			 ptstr
			 "|"
			 (cdr (assoc "node" lstoh))
		 )
		 F2
	  )
	)
	(close F2)
      )
      (princ "\no__o �޷����")
    )
  )

  ;;��� �յ� ���ݣ���ţ����ԣ�
  (defun GetDiameterData (x)
    (setq en	  (cadr x)
	  obj	  (vlax-ename->vla-object en)
	  keyList (list "id" "length" "lx" "diameter" "hole" "inbuilt")
	  ;;�޳���ŵ�����ֵ
	  xdata (assoc -3 (entget en (cdr keyList)) )
    )
    (list
      (vlax-curve-getStartPoint obj)
      (vlax-curve-getEndPoint obj)
      (if xdata
	(GetXData->diameter en keyList "|")
	(GetXData->diameter en (list "id") "|")
      )
    )
  )

  (setq ssPl (ssget "x" (list '(0 . "LWPOLYLINE") (cons 8 "080_A"))))
  (setq ssoh (ssget "x" (list '(0 . "insert") (cons 2 "0-Id"))))
  (if (null ssOh)
    (princ "\no__o ������ �̵߳� ����")
    (setq tfH t)
  )
  (if (null ssPl)
    (princ "\no__o ������ ��ͷ ����")
    (setq tfPl t)
  )
  (if (and tfH tfPl)
    (progn
      (princ (strcat "\no__o ��ȡ���� �߳� ����Ϊ:"
		     (itoa (sslength ssoh))
	     )
      )

      (setq
	lstPl (mapcar 'GetDiameterData (ssnameX ssPl))
	lstOh (mapcar
		'(lambda (x)
		   (cons (assoc 10 (entget (cadr x)))
			 (BlockGetAtts (cadr x))
		   )
		 )
		(ssnameX ssOh)
	      )
      )

      (Cad2txt_oh lstOh (nth 0 configExportDataFilePath))
      (Cad2txt_Arrow lstPl (nth 1 configExportDataFilePath))
    )
  )
  (princ "\no__o ִ�����")
  (princ)
  )

