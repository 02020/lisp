;; ����޸�ʱ�� 2018��2��10��

;| ����������:GGT
__ 1.���ݵ����ʸ����ͷ
__ 2.����ʸ����ͷ��ȡ�̵߳�����,���� Oh ��
__ 3.��ȫ�̵߳�,����߲�
__ 4.���� ��ͷ,��һ��˳���ʾ������
__ 5.�ϲ�ͬһ���Oh��

__ ��ע:���ǵ� lstpt ����Ϊ��ά�㣬�̵߳�Ϊ��ά����Ҫת��


__ 2.���� ʸ����ͷ ���� �����

__ 4.���� �����,��һ��˳���ʾ������
|;



 ;| ����������:GGT
__ 0.ϵͳ��ʼ��
__ 1.�������������
__ 2.���õ�ǰ�����Ĺ�����
__ 3.��������� ---- �����������㣬�ظ���
__ 4.����ͼ�����ݽڵ����ʲ��������Ŀ�
__ 5.

__ ��ע:���ǵ� lstpt ����Ϊ��ά�㣬�̵߳�Ϊ��ά����Ҫת��


__ 2.���� ʸ����ͷ ���� �����

__ 4.���� �����,��һ��˳���ʾ������
|;

;;������εݽ� Elevation->h(�߳�)->h(id)->attr

(defun Init (/ currentLayer)
  ;; (#MakeLayers configLayerList) ;����ͼ��
  (CreateBlockId)
  (CreateBlockElevation)
  (CreateBlockAttr configPoint)
)


(defun c:d()
  (MakeArrows)
  )


 ;| 1.������������� |;
(defun FetchPointIntoDwg (/ bmList layerNot layerName schedule block filePath)
  (setvar "osmode" 0)
  (setq blockSize "0.5")
  (setq filePath (GetFiles))
 ;(setq filePath "D:\\C3.CAD\\03.Code\\o���߳�ͼv1\\Data\\�ɹ�1\\���ջ�Էԭʼ��������.txt")

	  ;(vl-cmdf "UNDO" "BE")
    (if (null filePath) (exit))
  (setq dcl_id (load_dialog "scroll-bar"))
  (new_dialog "scrolling" dcl_id)

  (setq lst (ReadTxt filePath)
	n 0
	num (length lst)
	)

  (setq layerNot nil)

  (foreach line	lst
    ;;������
    (setq schedule (* 2.0 (/ n 2.0 num)))
    (scroll-bar schedule)

    (setq k (#sparser line ",")
	  h (nth 4 k) ;�߳�
	  n (1+ n)
	  x (nth 3 k)
	  y (nth 2 k)
    )

    ;;�����
    (if	(and (/= x "")(/= y "")  h)
      (progn
	(setq pt     (list (distof (cadddr k) 2) (distof (caddr k) 2))
	      bmList (StrDivEnInt (nth 1 k))
	)
	(setq block (InsertBlockAttr pt blockSize "0-Elevation" (list (nth 1 k) h)))

	(cond
	  ((= (length bmList) 3)
	   (setq dm (cadr bmList))
	  )
	  ((= (length bmList) 2)
	   (if (wcmatch (car bmList) "#")
	     (setq dm (cadr bmList))
	     (setq dm (car bmList))
	   )
	  )
	)
	(setq layerName (GetLayerFormNode dm))
	(if layerName
	  (vla-put-layer block layerName)
	  (if (not (member dm layerNot))
	    (setq layerNot (cons dm layerNot))
	  )
	)
      )
    )
  )
  (setq layStr (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) layerNot)))
  (princ (strcat "\no__o ���±��벻���ڶ�Ӧͼ�� " layStr))
  (princ "\no__o ����˶Խڵ����ڵ�ͼ��,�ֶ����е��� ")
  ;;ж�ؽ�����
  (done_dialog)
  (unload_dialog dcl_id)
(princ)
)

(defun GetLayerFormNode9 (bm)
  (setq color 1)
  (setq layerName (assoc bm configNodeSortList))
  (if (null layerName)
    nil
    (progn
      (setq layerName  (cadr layerName)
	    layerColor (assoc layerName configLayerList)
      )
      (if (not (null layerColor))
	(setq color (cadr layerColor))
      )
      (if (not (tblsearch "layer" layerName))
	(vla-put-Color (vla-add LayerSel layerName) color)
      )
      layerName
    )
  )
)

;;���ݽڵ��ȡͼ��
(defun GetLayerFormNode	(bm / layer layerName layerColor)
  (defun *error* (msg)(princ (strcat "\no__o GetLayerFormNode:" msg)))
  ;configLayerList configNodeList
  (setq 
    LayerSel (vla-get-Layers AcadDocument)
    layer    (assoc bm configLayerList)
  )
  (if (null layer)
    (progn nil)
    (progn
      (setq layerName  (cadr layer)
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


;|

	(if (not (null layerName))
	  (progn
	  (setq layerName (getstring (strcat "\no__o ��������� " bm " ����ͼ������� "))	    )
	    (vla-add LayerSel layerName)
	    (setq layerList (cons bm layerList)
	    )
	  )
	)

 |;



;; 1.���õ�ǰͼ��
;; currentLayer ȫ�ֱ���
(defun SetCurrentLayer ()
  (setq currentLayer (getstring "\no__o �����뵱ǰҪ�����Ĺ����㣺"))
  (setvar "CLAYER" currentLayer)
)


 ;|

    (VL-CMDF "INSERT" "0attr" pt "1" "1" "" (nth 1 k) h )
    (repeat (- (length configPoint) 1)
      (vl-cmdf "")
    )
    (VL-CMDF "change" (entlast) "" "p" "la" layer "")


|;








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

