;;�������Կ�
;;  (InsertBlockAttr (list 0 0 0) blockSize "0-Elevation" (list "dd" "da"))
(defun InsertBlockAttr (pt blockSize blockName blockAttrs)
  (defun *error* (msg)(princ (strcat "\no__o InsertBlockAttr :" msg)))
  (setq block (vla-insertblock mSpace (vlax-3d-point pt) blockName blockSize blockSize blockSize 0))
  (if (vla-get-HasAttributes block)
    (progn
      (setq attrs (vla-GetAttributes block)
	    attrs (vlax-safearray->list (vlax-variant-value attrs))
      )
      (if (/= (length attrs) (length blockAttrs))
	(princ "\n o__o �����б����")
	(progn
	  (setq	n   0
		len (length attrs)
	  )
	  (repeat len
	    (vla-put-TextString (nth n attrs) (nth n blockAttrs))
	    (setq n (1+ n))
	  )
	)
      )
    )
  )
  block
)

(defun c:GetPointFromBlockElevation ()
  (setq	pt (GetPointFromAttr "0-Elevation")
	pr (strcat (rtos (car pt)) "," (rtos (cadr pt)))
  )
  (princ (strcat "\no__o " pr))
  (princ)
)


;;��ȡ���Կ�ĵ�λ��
(defun GetPointFromAttr	(blockName)
  ;(defun *error* (msg)   (error "GetPointFromAttr" msg) )
  (princ "\no__o ѡ�����Կ�")
  (if (= blockName "")
    (setq blockName "*")
  )
  (setq ssk (ssget ":s" (list (cons 0 "insert") (cons 2 blockName))))
  (if ssk 
    (cdr (assoc 10 (entget (ssname ssk 0))))
    nil
  )
)






;;�����Ա༭
(defun BlockAttrEdit (ss newStr / sameset)
  (setq lstss1 (ss2lst ss))
  (setq ii -1)
  (repeat (length lstss1)
    (setq attlst1 (list "���             ����ֵ")
	  attlst2 (list "           ")
	  spcae	  "               "
    )
    (setq ENT1 (entget (nth (setq ii (1+ ii)) lstss1)))
    (while
      (= (cdr
	   (assoc 0
		  (setq ENT1 (entget (entnext (cdr (assoc -1 ENT1)))))
	   )
	 )
	 "ATTRIB"
      )
       (setq atte    (cdr (assoc -1 ENT1))
	     wz1     (cdr (assoc 1 ENT1))
	     Sign    (cdr (assoc 2 ENT1))

	     attlst1 (append
		       attlst1
		       (list (list Sign (substr spcae (strlen sign)) wz1))
		     )
	     attlst2 (cons atte attlst2)
       )
    )
    (if	(not sameset)
      (setq sameset "0")
    )
    (if	(and (= sameset "0") (> (length attlst1) 2))
      (progn ;���Բ�ֹ1�����ֶԻ���

;;;;;;;;;;;;;;�Ի���ʼ;;;;;;;;;;;;;;;;;;;
	(SETQ std3 3)
	(WHILE (> std3 0)
	  (attdetail)
	  (cond	((= std3 5)
;;;ȡ����ѡ����ֵ�б����
		 (setq attlst (str->lst attstr " "))
		 (if (= (car attlst) "0")
		   (setq attlst (cdr attlst))
		 )
		 (if (not attlst)
		   (progn (princ "\n���𵥶�ѡ���һ�б�ͷ!") (exit))
		 )
		 (setq selectedsign
			(mapcar
			  'car
			  (mapcar
			    '(lambda (x) (nth (atoi x) attlst1))
			    attlst
			  )
			)
		 )
;;;ȡ��ѡ��ı��
		 (IF (or (< (length attlst1) (atoi (last attlst)))
			 (< (length attlst1) (length attlst))
		     )
		   (setq oldattlst '("0"))
		   (setq oldattlst ATTLST)
		 )
		)
	  )
	)
	(COND
	  ((= std3 0)
	   (setq i 0)
	   (repeat (length attlst)
	     (if
	       (member
		 (car (setq newsign (nth (atoi (nth i attlst)) attlst1))
		 )
		 selectedsign
	       )
;;;�ж��Ƿ���ȡ�õı��һ��
		(UpdateAttr
		  (nth (atoi (nth i attlst)) (reverse attlst2))
		  newStr
		)
	     )
	     (setq i (1+ i))
	   )
	  )
	  ((= std3 -1) (unload_dialog id3) (term_dialog))
	)
;;;;;;;;;;;;;;;;�Ի������;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      )
      (cond
	((<= (length attlst1) 2)
;;;���Խ�1�������ֶԻ���
	 (UpdateAttr
	   (nth 1 (reverse attlst2))
	   (last (nth 1 attlst1))
	 )
	)
	((and (= sameset "1") (> (length attlst1) 2))
;;;���Բ�ֹ1�����Ѿ�ѡ������ʾʱ�����ֶԻ���
	 (setq attlst (str->lst attstr " "))
	 (if (= (car attlst) "0")
	   (setq attlst (cdr attlst))
	 )
	 (if (not attlst)
	   (progn (princ "\n���𵥶�ѡ���һ�б�ͷ!") (exit))
	 )
	 (setq i 0)
	 (repeat (length attlst)
	   (if
	     (member
	       (car (setq newsign (nth (atoi (nth i attlst)) attlst1)))
	       selectedsign
	     )
;;;�ж��Ƿ���ȡ�õı��һ��
	      (UpdateAttr
		(nth (atoi (nth i attlst)) (reverse attlst2))
		newStr
	      )
	   )
	   (setq i (1+ i))
	 )
	)
      )
    )
  )
)

;;ѡ��ת��
(defun ss2lst (ss / i e lst)
  (setq i -1)
  (repeat (sslength ss)
    (setq e   (ssname ss (setq i (1+ i)))
	  lst (cons e lst)
    )
  )
)


(defun str->lst	(str sign / position lst)
  (while (and str (/= str ""))
    (if	(setq position (vl-string-search sign str))
      (progn
	(setq lst (append lst (list (substr str 1 position))))
	(setq str (substr str (+ 2 position)))
      )
      (progn
	(setq lst (append lst (list str)))
	(setq str nil)
      )
    )
  )
  lst
)

;;���Ը���
(defun UpdateAttr (en x)
  (defun *error* (msg)(princ (strcat "\no__o UpdateAttr :" msg)))
  (setq ent (entget en))
  (setq ent (subst (cons 1 (eval x)) (assoc 1 ent) ent))
  (entmod ent)
  (entupd (cdr (assoc -1 ent)))
)




;;�����Ի������Կ�༭����
;;�༭ʱ���õı��� attlst  
(defun attdetail (/ fn3 lsdcl3)
  (setq	fn3 (open (setq lsdcl3 (VL-FILENAME-MKTEMP "tmp" "" ".dcl"))
		  "w"
	    )
  )
  (if (not attstr)
    (setq attstr "0")
    (progn (setq attstr "")
	   (mapcar '(lambda (x) (setq attstr (strcat attstr x " ")))
		   oldattlst
	   )
    )
  )
  (write-line "att:dialog{" fn3)
  (write-line "   label=\"����ѡ��\";" fn3)
  (write-line "   :boxed_row{label=\"ѡ������\";" fn3)
  (write-line
    "        :list_box{key=\"attlst\";fixed_width=true;width=41;multiple_select=false;}"
    fn3
  )
  (write-line "	}" fn3)
  (write-line "   :row{" fn3)
  (write-line
    "        :toggle{label=\"���������ѡ��\";key=\"sameset\";fixed_width=true;width=8;}"
    fn3
  )
  (write-line
    "        :button{label=\"ȷ��\";key=\"accept\";fixed_width=true;width=4;}"
    fn3
  )
  (write-line
    "        :button{label=\"ȡ��\";key=\"cancel\";is_cancel=true;fixed_width=true;width=4;}"
    fn3
  )
  (write-line "	}" fn3)
  (write-line "	}" fn3)
  (close fn3)
  (setq id3 (LOAD_DIALOG lsdcl3))
  (new_dialog "att" id3 "" screenpt3)
  (start_list "attlst" 2)
  ;;�����б�ʼ
  (mapcar
    'add_list
    (mapcar '(lambda (x) (vl-string-trim "()" (vl-princ-to-string x)))
	    attlst1
    )
  )
  ;;��ѡ�������������ӵ��б�
  (end_list)
  ;;����б����
  (set_tile "attlst" attstr)
  (set_tile "sameset" sameset)
  (action_tile
    "attlst"
    "(setq attstr $value)(setq screenpt3(done_dialog 5))"
  )
  ;;�����б�
  (action_tile
    "sameset"
    "(setq sameset $value)(setq screenpt3(done_dialog 6))"
  )
  (action_tile "accept" "(setq screenpt3(done_dialog))")
  ;;ȷ��
  (action_tile "cancel" "(setq screenpt3(done_dialog -1))")
  ;;ȡ��
  (setq std3 (START_DIALOG))
  (unload_dialog id3)
  (VL-FILE-DELETE lsdcl3)
  (if lsdcl3
    (VL-FILE-DELETE lsdcl3)
  )
)




;;��ʱ����
(defun EntmakeInsertB (name pt / E EN P10 STR)
  (defun MKATTRIB (pt str H)
    (entmake (list
	       '(0 . "ATTRIB")
	       '(100 . "AcDbEntity")
	       '(100 . "AcDbText")
	       (cons 10 pt)
	       (cons 40 H)
	       (cons 1 str)
	       '(100 . "AcDbAttribute")
	       (cons 2 str)
	       '(70 . 0)
	     )
    )
  )
  (setq e (TBLOBJNAME "Block" name))
  (setq pt (trans pt 1 0))
  (cond	((equal (assoc 70 (entget e)) '(70 . 0))
	 (entmake (list '(0 . "INSERT") (cons 2 name) (cons 10 pt)))
	)
	(T
	 (entmake (list '(0 . "INSERT") '(66 . 1) (cons 2 name) (cons 10 pt)))
	 (while	(setq e (entnext e))
	   (setq en (entget e))
	   (cond ((equal (assoc 0 en) '(0 . "ATTDEF"))
		  (setq p10 (mapcar '+ pt (cdr (assoc 10 en))))
		  (setq str (cdr (assoc 1 en)))
		  (MKATTRIB p10 str (cdr (assoc 40 en)))
		 )
	   )
	 )
	 (entmake '((0 . "SEQEND")))
	)
  )
  (entlast)
)




;;; �������Կ�-�߳�
(defun CreateBlockElevation ()
  (setq blockName "0-Elevation")
  (setq BlockInsertionPoint (vlax-make-safearray vlax-vbDouble '(0 . 2)))
  (vlax-safearray-fill BlockInsertionPoint '(0 0 0))
  (setq insertPnt (vlax-make-safearray vlax-vbDouble '(0 . 2)))
  (vlax-safearray-fill insertPnt '(0 0 0))
  (setq BlockSel (vla-get-blocks AcadDocument))
  (setq BlockObj (ObjSel (vla-get-blocks acaddocument) blockName))
  (if (/= BlockObj nil)
    (princ (strcat "\no__o " blockName " ͼ���Ѿ�����"))
    (progn
      (setvar "CLAYER" "0")
      (setq newBlock (vla-add BlockSel BlockInsertionPoint blockName))
      (setq circleObj (vla-AddCircle newBlock insertPnt 0.4))
      (setq circleObj (vla-AddCircle newBlock insertPnt 0.8))
      (setq
	height 1.00
	ptId   (list height (- 0 (/ height 1)) 0)
	ptH    (list height (- 0 (/ height 2)) 0)
      )
      (vlax-safearray-fill insertPnt ptId)
      (vla-AddAttribute newBlock (/ height 3) 0 "����" insertPnt "code" "00")
      (vlax-safearray-fill insertPnt ptH)
      (vla-AddAttribute newBlock height 0 "������߳�" insertPnt "h" "00.00")
      (princ (strcat "\no__o " blockName " ͼ�鴴���ɹ�"))
    )
  )
)


;;; �������Կ�-�߳�
(defun CreateBlockId ()
  (setq blockName "0-Id")
  (setq BlockInsertionPoint (vlax-make-safearray vlax-vbDouble '(0 . 2)))
  (vlax-safearray-fill BlockInsertionPoint '(0 0 0))
  (setq insertPnt (vlax-make-safearray vlax-vbDouble '(0 . 2)))
  (vlax-safearray-fill insertPnt '(0 0 0))
  (setq BlockSel (vla-get-blocks AcadDocument))
  (setq BlockObj (ObjSel (vla-get-blocks acaddocument) blockName))
  (if (/= BlockObj nil)
    (princ (strcat "\no__o " blockName " ͼ���Ѿ�����"))
    (progn
      (setvar "CLAYER" "0")
      (setq newBlock (vla-add BlockSel BlockInsertionPoint blockName))
      (setq circleObj (vla-AddCircle newBlock insertPnt 0.4))
      (setq circleObj (vla-AddCircle newBlock insertPnt 0.8))
      (setq
	height 1.00
	ptId   (list height (- 0 (* height 0.5)) 0)
	ptH    (list height (- 0 (* height 1.2)) 0)
	ptHc   (list height (- 0 (* height 2)) 0)
      )
      (vlax-safearray-fill insertPnt ptId)
      (vla-AddAttribute newBlock (/ height 1) 0 "����" insertPnt "id" "00")
      (vlax-safearray-fill insertPnt ptH)
      (vla-AddAttribute newBlock (/ height 2) 1 "������߳�" insertPnt "h" "00.00")
      (vlax-safearray-fill insertPnt ptHc)
      (vla-AddAttribute newBlock (/ height 2) 1 "������߲�" insertPnt "hc" "00.00")
      (vlax-safearray-fill insertPnt ptH)
      (vla-AddAttribute newBlock (/ height 2) 0 "������ڵ�����" insertPnt "node" "")

      (princ (strcat "\no__o " blockName " ͼ�鴴���ɹ�"))
    )
  )
)


;;�������Կ�-��
(defun CreateBlockAttr (attrList)
  (setq blockName "0-Attr")
  (setq BlockSel (vla-get-blocks acaddocument))
  (setq BlockObj (ObjSel blockSel blockName))
  (setq BlockInsertPnt (vlax-make-safearray vlax-vbDouble '(0 . 2)))
  (setq insertPnt (vlax-make-safearray vlax-vbDouble '(0 . 2)))
  (vlax-safearray-fill insertPnt '(0 0 0))
  (vlax-safearray-fill BlockInsertPnt '(0 0 0))
  (setq layerSel (vla-get-Layers AcadDocument))
  (setq layerObj (vla-add layerSel "080_k"))
  (vla-put-Color layerObj acYellow)
  (if (/= BlockObj nil)
    (princ (strcat "\no__o " blockName " ͼ���Ѿ�����"))
    (progn (setvar "CLAYER" "0")
	   (setq BlockSel (vla-get-blocks AcadDocument))
	   (setq newBlock (vla-add BlockSel BlockInsertPnt blockName))
	   (vlax-safearray-fill insertPnt '(0 0 0))
	   (setq radius 0.1)
	   (setq circleObj (vla-AddCircle newBlock insertPnt radius))
	   (setq radius 0.3)
	   (setq circleObj (vla-AddCircle newBlock insertPnt radius))
	   (vlax-safearray-fill insertPnt '(0.5 0 0))
	   (setq height 0.4)
	   (setq mode acAttributeModeNormal)
	   (vla-AddAttribute newBlock height mode "���" insertPnt "���" "00")
	   (setvar "CLAYER" "080_k")
	   (setq i	0
		 height	(* height 0.5)
	   )
	   (foreach attr attrList
	     (setq i (- i 0.5))
	     (vlax-safearray-fill insertPnt (list 0.5 i 0))
	     (setq value   ""
		   tag	   (cadr attr)
		   printer (strcat "" tag)
	     )
	     (vla-AddAttribute
	       newBlock	height mode printer insertPnt tag value)
	   )
	   (princ (strcat "\no__o " blockName " ͼ�鴴���ɹ�"))
    )
  )
)

;;(setq blockRefObj (vla-InsertBlock mSpace insertPnt "0h" 0.5 0.5 0.5 0))


;;; ����ͼ��
(defun CreateBlack_oh ()
  (setq insertPnt (vlax-make-safearray vlax-vbDouble '(0 . 2)))
  (vlax-safearray-fill insertPnt '(0 0 0))
  (setq BlockSel (vla-get-blocks acaddocument))
  (setq BlockObj (ObjSel blockSel "0-h"))

  (setq layerSel (vla-get-Layers AcadDocument))
  (setq layerObj (vla-add layerSel "080_k"))
  (vla-put-Color layerObj acYellow)

  (if (/= BlockObj nil)
    (princ "\no__o 0h ͼ���Ѿ�����,������ִ��")
    (progn
      (setvar "CLAYER" "0")
      (setq BlockInsertionPoint
	     (vlax-make-safearray vlax-vbDouble '(0 . 2))
      )
      (vlax-safearray-fill BlockInsertionPoint '(0 0 0))
      (setq BlockSel (vla-get-blocks AcadDocument))
      (setq newBlock (vla-add BlockSel BlockInsertionPoint "0-h"))
      (vlax-safearray-fill insertPnt '(0 0 0))
      (setq radius 0.1)
      (setq circleObj (vla-AddCircle newBlock insertPnt radius))
      (setq radius 0.2)
      (setq circleObj (vla-AddCircle newBlock insertPnt radius))
;;; ����ID���Զ���
      (vlax-safearray-fill insertPnt '(1 0 0))
      (setq height 1)
      (setq mode acAttributeModeNormal)
      (setq printer "����� ")
      (setq tag "ID")
      (setq value "00")
      (setq AttributeObj
	     (vla-AddAttribute
	       newBlock	height mode printer insertPnt tag value)
      )
      (vlax-safearray-fill insertPnt '(1 -1 0))
;;; ����h1���Զ���
      (setvar "CLAYER" "080_k")
      (setq printer "������߳� ")
      (setq height (* height 0.6))
      (setq tag "h1")
      (setq value "00.00")
      (setq AttributeObj
	     (vla-AddAttribute
	       newBlock	height mode printer insertPnt tag value)
      )
;;; ����h2���Զ���
      (vlax-safearray-fill insertPnt '(1 -2 0))
      (setq printer "������߲� ")
      (setq tag "h2")
      (setq AttributeObj
	     (vla-AddAttribute
	       newBlock	height mode printer insertPnt tag value)
      )

      (vlax-safearray-fill insertPnt '(0 0 0))

      (princ "\no__o 0h ͼ�鴴���ɹ�")
    )
  )
)



