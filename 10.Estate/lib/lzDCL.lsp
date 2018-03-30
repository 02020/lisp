;|
  "4.��ͷ->��� MarkOrderPoint4Arrows "

|;
;;����������Ψһ��

;| �������� ���� |;
(defun c:cg (/ dclst)
  (setq dclst '(("����"
                 ("0.��ʼ�� Init "
                  "1.�������� FetchPointIntoDwg "
                  "2.ɾ���ظ� EraseRepeatData"
                  "���ù���ͼ�� SetCurrentLayer"
                  "3.����(����)(&D) MakeArrows "
                  "4.��ȡ�߳� AddBlockH "
		  "5.���ýڵ����� SetBlockNodeAttr"
		  "6.���ɽڵ���� GetOrderId"
                  "7.�ϲ�ID�� CombineBlockId"
                  "D.������� ExportDataNodeArrow "
                 )
                )

                ("�ܾ�����"
                 ("ID-���ùܾ���� SetArrowId"
                  "ID-������߱�� InsertArrowId"
                  "ID-��ʾ�ܾ�ID ShowArrowId"
                  "ID-���عܾ�ID EraseArrowId"
		   "�ܾ����� ~002 "
                  "D-���ùܾ� SetArrowData"
                  "D-���ùܾ� ResetArrowId"
                  "D-��ʾ�ܾ� ShowArrowText"
                 )
                )

;;;                ("B ����"
;;;                 ( ;"��� ~JC"
;;;                  "C.�˲�_�ڵ�� CheckBlockId "
;;;                  "C.�˲�_��ͷ CheckArrow "
;;;		  "C.��ͷ���� CheckArrowLen"
;;;                  "�鿴_�ܾ� ShowXDataDis "
;;;                  "�ڵ����� InsertPointBlock"
;;;                  "�ĵ����� ~12 "
;;;                  "1.Oh->�ı� Oh->Text"
;;;                  "2.����ͷ->����� Arrows2Pline "
;;;                  "3.���ظ������� layerFreeze_080 "
;;;                 )
;;;                )

;;;                ("����"
;;;                 ("1.�����߶γ��� SumCurveLen "
;;;                  "2.������߳��� CheckPlineLen "
;;;                  "3.��ȡ��� GetPointFromBlockElevation"
;;;                 )
;;;                )
               )
  )
  (while
    (Dcl-ButtonM "�����߹����䡹v1" "����˵��\n������Ҫ���" dclst nil)
  )
  (princ)
)


 ;| ������ ���� |;
(defun c:cgt (/ dcllst)
  (setq dcllst '(
                 ("A �߳����"
                  ("1.����ʸ����ͷ MakeArrows "
                   "2.��ȡ�߳� AddBlockOh "
                   "3.����Oh��(&D) InsertBlock_oh "
                   "4.��ͷ->��� MarkOrderPoint4Arrows "
                   "4.Pl->��� MarkOrderPoint4Plines "
                   "5.Oh->ID�ϲ� CombineBlockAuto->oh "
                   "6.�ܾ�->��ͷ PutXDataText->Pline "
                   "7.��ͷ->�ܾ� MarkArrow->Dis "
;;;					"�� ~k "
;;;					".����_0h_ͼ�� CreateBlack_oh "
                  )
                 )
                 ("B ����"
                  ( ;"��� ~JC"
                   "C.�˲�_Oh�� CheckOh "
                   "C.�˲�_��ͷ CheckArrowOh "
                   "�鿴_�ܾ� ShowXDataDis "
                   "D.������� ohArrow_Cad2txt "
                   ""
                   "�ĵ����� ~12 "
                   "1.Oh->�ı� Oh->Text"
                   "2.����ͷ->����� Arrows2Pline "
                   "3.���ظ������� layerFreeze_080 "
                  )
                 )
                 ("����"
                  ("1.�ϲ�TXT���� hbtxt "
                   "2.�ϲ�ֱ�������� PlineJoinline "
                   "3.�ϲ������ PlineJoin "
                   "4.��϶���� PlineAutoBreak "
                   "5.��ת����� RevPline "
                   "6. ~1 "
                  )
                 )
                 ("����"
                  (
                   "1.�����߶γ��� SumCurveLen "
                   "2.������߳��� CheckPlineLen "
                   "4.����߳� CalculateHight "
                   "0.������ ~MakeArrows "
                  )
                 )
                )
  )
  (setq strHelp (strcat
                  "��������\n"     "\n1.��������"   "\n2.��ȡ�̵߳�"
                  "\n3."           "\n4."           "\n5."
                 )
  )
  (while (Dcl-ButtonM "�����߹����䡹v1" strHelp dcllst nil))
  (princ)
)

 ;|Dcl-ButtonM
__
	titl����
	hltxt����˵��
	buttons��ť�б�
	flag����ʱ���к���T���к���
__
|;

(defun Dcl-ButtonM (titl hltxt buttons flag / a b c d dcl)
  (defun strsplist (str / i)
    (if (setq i (vl-string-search " " str))
      (list (substr str 1 i)
            (vl-string-trim " " (substr str (+ 2 i)))
      )
    )
  )
  (defun makedcl (str_lst / dclfile fileID dclHandle)
    (setq dclfile (vl-filename-mktemp nil nil ".dcl")
          fileID  (open dclfile "w")
    )
    (cond ((= (type str_lst) 'str) (write-line str_lst fileID))
          ((= (type str_lst) 'list)
           (foreach n str_lst (write-line n fileID))
          )
    )
    (close fileID)
    (setq dclHandle (load_dialog dclfile))
    (vl-file-delete dclfile)
    dclHandle
  )
  (setq b (if flag
            ":column{label = \""
            ":row{label = \""
          )
        c (if flag
            ":row{label = \""
            ":column{label = \""
          )
        d '("ESC")
        a (strcat (vl-string-translate
                    "$~"
                    "AB"
                    (vl-filename-base (vl-filename-mktemp))
                  )
                  ":dialog{label=\""
                  titl
                  "\";"
                  b
                  "\";"
          )
  )
  (foreach x buttons
    (if (listp x)
      (progn (setq a (strcat a c (car x) "\";"))
             (foreach y (last x)
               (setq b (reverse (strsplist y))

                     a (if b
                         (strcat a
                                 ":button {key =\""
                                 (car b)
                                 "\";label=\""
                                 (last b)
                                 "\";}"
                         )
                         (strcat a "spacer;")
                       )
               )
               (setq d (if b
                         (cons (car b) d)
                         d
                       )
               )
             )
             (setq a (strcat a "}\n"))
      )
      (setq a (strcat a "spacer;\n"))
    )
  )
  (setq d (REVERSE d)
        a (strcat
            a
            ""
            (if flag
              ":row"
              ":column"
            )
            "{spacer_0;:button{key = \"HLP\";label = \"����\";}"
            ":button{key=\"ESC\";label=\"ȡ��(&Q)\";is_cancel = true;}}}} "
          )
  )
  (setq dcl (makedcl a))
  (NEW_DIALOG (substr a 1 8) dcl)
  (action_tile "ESC" "(done_dialog 0)")
  (action_tile
    "HLP"
    (strcat "(alert \""
            (if hltxt
              hltxt
              "û�й���˵��"
            )
            "\")"
    )
  )
  (foreach a (cdr d)
    (if a
      (progn (if (= (substr a 1 1) "~")
               (mode_tile a 1)
             )
             (action_tile
               a
               (strcat "(done_dialog " (itoa (vl-position a d)) "))")
             )
      )
    )
  )
  (setq a (nth (START_DIALOG) d))
  (UNLOAD_DIALOG dcl)
  (if (/= "ESC" a)
    (progn
      (cond ((vl-remove 'nil (atoms-family 1 (list (strcat "c:" a))))
             (eval (read (strcat "(c:" a ")")))
            )
            ((vl-remove 'nil (atoms-family 1 (list a)))
             (eval (read (strcat "(" a ")")))
            )
            (t (alert (strcat "���ܺ���" a "������")))
      )
      t
    )
  )
)

