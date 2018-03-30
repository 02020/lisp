;|______________________________________________________________________
	���еĹ��ߺ���
	������������
	����ʱ��ʼ������
	ȫ�ֱ���
	����

______________________________________________________________________|;

;;;--------------------------------------------------------------------;
;;;  First step is to load ActiveX functionality.  If ActiveX support  ;
;;;  already exists in document (can occur when Bonus tools have been  ;
;;;  loaded into AutoCAD), nothing happens.                            ;
;;;--------------------------------------------------------------------;

(progn
  (vl-load-com)
  (vl-load-reactors)
  (setvar "cmdecho" 0)
)
;;;--------------------------------------------------------------------;
;;;  For ActiveX functions, we need to define a global variable which  ;
;;;  "points" to the Model Space portion of the active drawing.  This  ;
;;;  variable, named *ModelSpace* will be created at load time.        ;
;;;--------------------------------------------------------------------;
(setq *ModelSpace*
		   (vla-get-ModelSpace
		     (vla-get-ActiveDocument (vlax-get-acad-object))
		   )
      AcadObject   (vlax-get-acad-object)
      AcadDocument (vla-get-ActiveDocument AcadObject)
      mSpace	   (vla-get-ModelSpace AcadDocument)

 ;_ end of vla-get-ModelSpace
) ;_ end of setq



(defun *error* (msg)
  (error "" msg)
)

(defun error (fnName msg)
  (if (wcmatch (strcase msg t) "*break,*cancel*,*exit*,*ȡ��*")
    (progn (princ "\no__o �����˳�..."))
    (princ (strcat "\no__o " fnName ":" msg))
  )
)


;;;(defun *error* (msg)
;;;  (if (wcmatch (strcase msg t) "*break,*cancel*,*exit*,*ȡ��*")
;;;    (princ)
;;;    (vla-EndUndoMark
;;;      (vla-get-ActiveDocument (vlax-get-acad-object))
;;;    )
;;;  )
;;;  (princ)
;;;)




;;;;������������������������������������������������������������
;;;;������                �������ֵ��������              ������
;;;;������                ��ӡ-����ֵ                     ������
;;;;������________________________________________________������

(defun e-ShowList(strlst / n4 n5 n6 text T40)
  (princ "\n���������ֵ��������" )
  (setq n4 -1 n5 1 n6 0)

  (setq col 5
   spjj 6)
  (while (null (setq pt (getpoint "\n��ѡ��ע��λ��:"))))
  (setq ptt pt czjj 1)
  (setq T40 0.5)  
  (command "undo" "be")
  (while (setq text (nth (setq n4 (1+ n4)) strlst))
    (if (> n4 (- (* col n5) 1) ) (setq ptt (polar pt (* pi 1.5) (* czjj n5)) n5 (1+ n5) n6 0) )
    (entmake (list '(0 . "TEXT") '(8 . "5˵��ע��") (cons 10 (polar ptt 0 (* spjj n6))) '(40 . 0.5) (cons 1 text)))       
    (setq  n6 (1+ n6))
    )
  (command "undo" "e")
  (princ))



;;;;������������������������������������������������������������
;;;;������                ���ַ����ָ�ɱ�                ������
;;;;������________________________________________________������

(defun e-ParseDelim (str delim / ptr lst)
  (setq lst '())
  (setq len (+ (strlen delim) 1))
  (while (setq ptr (vl-string-search delim str))
    (setq lst (cons (substr str 1 ptr) lst ))
    (setq str (substr str (+ ptr len)))
    )
  (reverse (cons str lst))
  )


(defun e-ParseString (st / stl lst i ii key)
  (setq	lst '()	i 1 ii 1)
  (setq	stl (strlen st) )
  (repeat stl
    (if (or (wcmatch (substr st i 1) "#" )
	    (wcmatch (substr st i 2) "[+-.e]#,e[+-],[+-].")  
         )
        (setq key "nom")
        (setq key "str")
     );end if 
    (if (= 1 i) (setq key2 key))
    (cond
       ((= stl i)(setq lst (cons (substr st ii (- (1+ i) ii)) lst)) )
       ((/= key2 key)
        (setq lst (cons (substr st ii (- i ii)) lst)
              ii i
              key2 key
        ))
     )
    (setq i (1+ i))    
  )
(setq lst (reverse lst))
)

(defun e-ParseStringAll (st / stl lst i ii key)
  (setq	lst '()	i 1 ii 1)
  (setq	stl (strlen st) )
  (repeat stl
    (if (wcmatch (substr st i 1) "#" ) (setq key "nom") (setq key "str"))
    (if (= 1 i) (setq key2 key)) ;�ж��״�ѭ��
    (if (/= key2 key)
      (setq lst (cons (substr st ii (- i ii)) lst)
	    ii i
	    key2 key
	    )
      )       
    (if (= stl i)(setq lst (cons (substr st ii (- (1+ i) ii)) lst)) )
    (setq i (1+ i))    
  )
(setq lst (reverse lst))
)

(defun e-ParseStringEvery (st / stl lst i ii key)
  (setq	lst '()	i 1 FT T)
  (while FT
    (if (wcmatch (substr st i 1) "#,@" ) (setq key 1) (setq key 2))    
    (setq lst (cons (substr st i key) lst))   
    (setq i (+ i key))
    (if (< (strlen st) i) (setq FT nil))
  )
(setq lst (reverse lst))
)




;;;--------------------------------------------------------------------;
;;;     Function: getPerp-Distance-and-Angle                           ;
;;;--------------------------------------------------------------------;
;;;  Description: This function returns a list with the distance and   ;
;;;               perpendicular angle to user pt3, and is determined   ;
;;;               by supplied points pt1 pt2.  Pt3 is "user input"     ;
;;;               and need not be at right angles.  This allows us to  ;
;;;               solve for cases where ortho mode is off.             ;
;;;  Example usage:                                                    ;
;;;        (setq Data  (getPerp-Distance-and-Angle pt1 pt2 pt3) )      ;
;;;--------------------------------------------------------------------;
;;;      Arguments:                                                    ;
;;;          pt1  seed point                                           ;
;;;          pt2  seed point                                           ;
;;;      Note:  pt1 and pt2 denote a "line" segment                    ;
;;;          pt3  "user point" (point to solve for)                    ;
;;;--------------------------------------------------------------------;

;|
__
__
__
|;

(defun lz-highlight (enCo)
  (mapcar '(lambda (x)
	     (vla-highlight (vlax-ename->vla-object x) :vlax-true)
	   )
	  enCo
  )
)

;;; �����µ�ͼ��
	  ; (#MakeLayers (list '("0_659" 1) '("0_665" 2) '("0_669" 3)))
(defun #MakeLayers (lst)
  (setq	AcadObject   (vlax-get-acad-object)
	AcadDocument (vla-get-ActiveDocument AcadObject)
	mSpace	     (vla-get-ModelSpace AcadDocument)
  )
  (setq LayerSel (vla-get-Layers AcadDocument))
  (mapcar '(lambda (x)
	     (vla-put-Color (vla-add LayerSel (car x)) (cadr x))
	   )
	  lst
  )
)

	  ;�ַ����к���List�е�Ԫ��
(defun IsExistStringInList (sList string i)
  (apply 'or
	 (mapcar
	   '(lambda (x)
	      (equal (nth (1- i) x) string)
	    )
	   sList
	 )
  )
)

(defun ReadTxt (filepath / lst)
  (setq	lst  nil
	file (open filepath "r")
  )
  (while (setq line (read-line file))
    (setq lst (cons line lst))
  )
  (close file)
  (reverse lst)
)



;;����ͼ�㼰����ɾ��ȫ���Ŀ�
(defun DelAllBolck (blockName layerName)
  (setq
    ss (ssget
	 "x"
	 (list '(0 . "insert") (cons 2 blockName) (cons 8 layerName))
       )
  )
  (if ss
    (command "_ERASE" ss "")
  )
)

;;;;������������������������������������������������������������
;;;;������                ���ַ����ָ�ɱ�                ������
;;;;������________________________________________________������

(defun StrDivEnInt (st / stl lst i ii key)
  (setq	lst '()
	i   1
	ii  1
	stl (strlen st)
  )
  (repeat stl
    (if	(or (wcmatch (substr st i 1) "#")
	)
      (setq key "integer")
      (setq key "string")
    )
    (if	(= 1 i)
      (setq key2 key)
    )
    (if	(/= key2 key)
      (setq lst	 (cons (substr st ii (- i ii)) lst)
	    ii	 i
	    key2 key
      )
    )
    (if	(= stl i)
      (setq lst (cons (substr st ii (- (1+ i) ii)) lst))
    )
    (setq i (1+ i))
  )
  (setq lst (reverse lst))
)






	  ;�ļ�ѡ��Ի�
(defun GetFiles	(/)
  (if (/= (vl-registry-read "HKEY_CLASSES_ROOT\\Licenses\\4D553650-6ABE-11cf-8ADB-00AA00C00905")
	  "gfjmrfkfifkmkfffrlmmgmhmnlulkmfmqkqj"
      )
    (vl-registry-write "HKEY_CLASSES_ROOT\\Licenses\\4D553650-6ABE-11cf-8ADB-00AA00C00905"
		       ""
		       "gfjmrfkfifkmkfffrlmmgmhmnlulkmfmqkqj"
    )
  )
  (if (setq x (Vlax-Get-Or-Create-Object "MSComDlg.CommonDialog"))
    (progn (vlax-put-property x "DialogTitle" "��ѡ����������")
	   (vlax-put-property x "Filter" "DWG Files|*.txt|All Files|*.*")
	   (vlax-put-property x "MaxFileSize" 10000)
	  ;        (vlax-put-property x "Flags" 512)
	   (vlax-put-property x "Flags" 1574404)
	   (vlax-put-property x "Action" 1)
	   (vlax-get x "Filename")
    )
    (princ "\no__o: GetFiles ��ȡ�ļ�·��ִ��ʧ�ܣ�")
  )
)




;;��ʽ����    
(defun FormatPoint (point num)
  (mapcar
    '(lambda (x)
       (fixqw x num) ;��ȡλ
     )
    point
  )
)


;;���ӵ�Բ����ǿ��λ�õ���ʾ
(defun AddCircle (pt pointCircle layer)
;;;  (setq layerSel (vla-get-Layers AcadDocument))
;;;  (setq layerObj (vla-add layerSel "080_0"))
  (setq	obj (vla-addCircle
	      (vlex-ModelSpace)
	      (vlax-3D-Point pt)
	      pointCircle
	    )
  )
  (vla-Put-Layer obj layer)
  (vla-Update obj)
  (vlax-Release-Object obj)
)

(defun scroll-bar (perc / icon_x icon_y)
  (setq	icon_x (dimx_tile "perc")
	icon_y (dimy_tile "perc")
  )					;setq
  (start_image "perc")
  (fill_image 0 0 (fix (* icon_x perc)) icon_y 1)
  (end_image)
)
 ;;ѡ��ת��Ϊ��
(defun ss->enlist (ss / lst n en)
  (setq n -1)
  (while (setq en (ssname ss (setq n (1+ n))))
    (setq lst (cons en lst))
  )
)


;;;;������������������������������������������������������������
;;;;������  #sparser  ����������ַ����ַ����ָ���ɱ�    ������
;;;;������       str  Ϊ��Ҫ������ַ���                  ������
;;;;������     delim  �ַ�������ָ��ַ�(��������)        ������
;;;;������------------------------------------------------������
;;;;������  #sparser2                                     ������
;;;;������     delim  �ַ�������ָ��ַ�(�����ַ�)        ������
;;;;������________________________________________________������
(defun #sparser	(str delim / ptr lst)
  (setq lst '())
  (setq len (+ (strlen delim) 1))
  (while (setq ptr (vl-string-search delim str))
    (setq lst (cons (substr str 1 ptr) lst))
    (setq str (substr str (+ ptr len)))
  )
  (reverse (cons str lst))
)
 ;|
__ ��ȡ���������
__ Param ssget
__ Return (list en ��� �յ�)
__ ĿǰӦ��:��ȡʸ����ͷ����
|;
(defun ssPl->lst (ss / n1 lstss x plObj)
  (setq	n1 -1
	lstss nil
  )
  (while (setq ssna (ssname ssPl (setq n1 (1+ n1))))
    (setq lstss (cons ssna lstss))
  )
  (mapcar '(lambda (x)
	     (setq plObj (vlax-ename->vla-object x))
	     (list
	       x
	       (vlax-curve-getStartPoint plObj)
	       (vlax-curve-getEndPoint plObj)
	     )
	   )
	  lstss
  )
)

;;;	(mapcar '(lambda (x)(setq PlCoAll
;;;	(vl-remove x PlCoAll))) PlCo)

(defun InsertEn	(pos lstI lstD / lstN)
  (setq lstN nil)
  (foreach n lstD
    (setq lstN (cons n lstN))
    (if	(= (vl-position n lstD) pos)
      (setq lstN (cons lstI lstN))
    )
  )
  (reverse lstN)
)


(defun InsertPt	(pos lstI lstD / lstN)
  (setq lstN nil)
  (foreach n lstD
    (setq lstN (append (list n) lstN))
    (if	(= (vl-position n lstD) pos)
      (setq lstN (append (reverse lstI) lstN))
    )
  )
  (reverse lstN)
)




;;��lst1��ɾ�Ƴ�lst2
(defun th-list-inter (lst1 lst2 / lst tmp)
  (setq lst '())
  (foreach tmp lst1
    (if	(not (member tmp lst2))
      (setq lst (cons tmp lst))
    )
  )
  (reverse lst)
)


(defun th-list-only (lst / lstN)
  (mapcar '(lambda (x)
	     (if (not (member x lstN))
	       (setq lstN (cons x lstN))
	     )
	   )
	  lst
  )
  lstN
)
	  ;��ȡ��������ж�������
(defun th2-get-allvertexs (e / n lst)
  (repeat (setq n (fix (1+ (vlax-curve-getendparam e))))
    (setq lst (cons (vlax-curve-getpointatparam e (setq n (1- n))) lst))
  )
)


	  ;��ȡ�ظ���Ԫ��
(defun same (l1 / l2)
  (while l1
    (if	(member (car l1) (cdr l1))
      (setq l2 (append l2 (list (car l1))))
    )
    (setq l1 (vl-remove (car l1) l1))
  )
  l2
)




(defun lstRe (ptt lstD / lst1 lst2)
  (if (= (vl-position ptt lstD) 0)
    (reverse (vl-remove ptt lstD))
    (progn
      (foreach n lstD
	(if (< (vl-position n lstD) (vl-position ptt lstD))
	  (setq lst1 (append (list n) lst1))
	  (setq lst2 (append (list n) lst2))
	)
      )
      (append (reverse lst1) (reverse lst2))
    )
  )
)






(defun #copy_ss0 (ss layer px) ;F:��ǰ B:���
  (if (null ss)
    (princ "�޶���")
    (command "copy" ss "" "@" "@" "change" ss "" "p" "la" layer "" "_.Draworder" ss "" px)
  )
  (princ "ִ�����")
)



;;;����������������������������������������
;;;���� #Entmake_Text                    ��
;;;����������������������������������������
;;;(defun #Entmake_Text (Text pt T40)
;;;  (if (or (= (type text) 'INT) (= (type text) 'REAL))
;;;	(setq text (rtos text 2 2))
;;;  )
;;;  (if (null Text)
;;;	(setq Text "nil")
;;;  )
;;;  (entmake (list '(0 . "TEXT")
;;;				 (cons 10 pt)
;;;				 (cons 40 T40)
;;;				 (cons 1 Text)
;;;		   )
;;;  )
;;;)


(defun #Entmake_Text (Text pt T40 T50)
  (if (or (= (type text) 'INT) (= (type text) 'REAL))
    (setq text (rtos text 2 2))
  )
  (if (null Text)
    (setq Text "nil")
  )
  (entmake (list '(0 . "TEXT")
		 (cons 10 pt)
		 (cons 40 T40)
		 (cons 1 Text)
		 (cons 50 T50)
	   )
  )
)






;;;����������������������������������������
;;;���� #Entmake_pline �����            ��
;;;����������������������������������������
(defun #EntMake_pline (lst / k lstPt)
  (setq k (cdr (assoc 40 lst)))
  (setq lstPt (cdr (assoc 10 lst)))
  (entmakeX
    (append
      (list '(0 . "LWPOLYLINE")
	    '(100 . "AcDbEntity")
	    '(100 . "AcDbPolyline")
	    (cons 90 (length (cdr (assoc 10 lst))))
	    (cons 8 (cdr (assoc 8 lst)))
      )
      (apply 'append
	     (mapcar '(lambda (x)
			(list (cons 10 x) (cons 40 k) (cons 41 k) '(42 . 0.0))
		      )
		     lstPt
	     )
      )
    )
  )
)


(defun #EntMake_pline_k	(lst / PT)
  (entmakeX
    (append (list '(0 . "LWPOLYLINE")
		  '(100 . "AcDbEntity")
		  '(100 . "AcDbPolyline")
		  (cons 90 (length (car lst)))
		  (cons 8 (cadr lst))
	    )
	    (apply 'append
		   (mapcar '(lambda (x)
			      (list (cons 10 (car x))
				    (if	(null (cadr x))
				      (cons 40 0)
				      (cons 40 (cadr x))
				    )
				    (if	(null (cadr x))
				      (cons 41 0)
				      (progn
					(if (null (caddr x))
					  (cons 41 (cadr x))
					  (cons 41 (caddr x))
					)
				      )
				    )
				    '(42 . 0.0)
			      )
			    )
			   (car lst)
		   )
	    )
    )
  )
)






 ;|______________________________________________________________________

  ��������
1 ��ÿ�����
2 ���ÿ�����
3 �ж� ���� �Ƿ����
________________________________________________________________________
								      |;

 ;| ��ÿ����� |;
(defun BlockGetAtts (ent / lst blkref a)
  (vl-load-com)
  (if (= (vla-Get-ObjectName
	   (setq blkref (vlax-Ename->vla-Object ent))
	 )
	 "AcDbBlockReference"
      )
    (if	(vla-Get-HasAttributes blkref)
      (progn
	(setq lst (vlax-safearray->list
		    (vlax-variant-value (vla-GetAttributes blkref))
		  )
	)
	(setq lst (mapcar 'cons
			  (mapcar 'vla-Get-TagString lst)
			  (mapcar 'vla-get-TextString lst)
		  )
	)

      )
    )	  ; endif
  )	  ; endif
  lst
)


 ;| ���ÿ����� |;
(defun BlockSetAtts (Obj lst / AttVal)
  (mapcar
    '(lambda (Att)
       (if (member (vla-get-TagString Att) lst)
	 (vla-put-TextString Att (cadr lst))
       )
     )
    (vlax-invoke Obj "GetAttributes")
  )
  (vla-update Obj)
  (princ)
)




 ;| �ж� ���� �Ƿ����
1 cltns: (vla-get-layers (vla-get-ActiveDocument (vlax-get-acad-object))))
2 cltns: (vla-get-blocks (vla-get-ActiveDocument (vlax-get-acad-object))))
3 objName ������
|;
(defun ObjSel (cltns objName / temp)
  (VLAX-FOR Obj	cltns
    (if	(vlax-property-available-p Obj 'Name)
      (if (= (vla-get-Name Obj) objName)
	(setq temp Obj)
      )
    )
  )
  (eval temp)
)


(defun BlokcIsExist (strName /)
  (setq BlockSel (vla-get-blocks acaddocument))
  (ObjSel blockSel strName)
)





 ;| ȡλ |;
(defun fixqw (num power)
  (setq power (expt 10 (- power 0)))
  (/ (float (fix (* num power))) power)
)




 ;| ����ѡ����ͼԪ�ĵ��б� ΪΨһֵ |;
 ;|
__	( ( '(��1) '(��2) )	( '(��1 en1) '(��1 en2) ) )
|;
(defun lstss->lstPt (lstss / _pt lstPt lstPtx)
  (setq	lstPtx (mapcar '(lambda	(x)
			  (setq _pt (cdr (assoc 10 (entget x))))
			  (list (list (car _pt) (cadr _pt) 0) x)
			)
		       lstss
	       )
  )
  (mapcar '(lambda (x)
	     (setq x (car x))
	     (if (not (member x lstPt))
	       (setq lstPt (cons x lstPt))
	     )
	   )
	  lstPtx
  )
  (list lstPt lstPtx)
)

 ;| ��en���ϸ��ݵ����(�ݹ�) |;
(defun lstenPt->Group (lsten lstRec / en pt lstD lstRe)
  (setq	en    (car lsten)
	lsten (cdr lsten)
	pt    (cdr (assoc 10 (entget en)))
	pt    (list (car pt) (cadr pt) 0)
	lstD  nil
	lstD  (cons en lstD)

  )
  (foreach x lsten
    (setq _pt (cdr (assoc 10 (entget x))))
    (setq _pt (list (car _pt) (cadr _pt) 0))
    (if	(equal _pt pt 0.0001)
      (progn
	(setq lsten (vl-remove x lsten))
	(setq lstD (cons x lstD))
      )
    )
  )
  (setq lstRec (cons lstD lstRec))

  (if (null lsten)
    lstRec
    (lstenPt->Group lsten lstRec)
  )
)




 ;|
rtos ������ת�����ַ��� (rtos number [mode [precision]])
itoa ������ת�����ַ�����������ת�����
atof ��һ���ַ���ת����ʵ�� (atof string)
atoi ��һ���ַ���ת�������� (atoi string)
distof ��һ����ʾʵ�����㣩�����ַ���ת����һ��ʵ��
rem ����һ�������Եڶ�����������������
minusp ���ĳ�����Ƿ��Ǹ���
|;



 ;| �����������ת��Ϊ�ַ����б�  |;
(defun str2lst (str /)
  (read	(vl-list->string
	  (apply 'append
		 (mapcar
		   '(lambda (x)
		      (if (= 32 x)
			(list 34 32 34)
			(list x)
		      )
		    )
		   (append (list 40 34) (vl-string->list str) (list 34 41))
		 )
	  )
	)
  )
)












