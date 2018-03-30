;|
  "4.箭头->序号 MarkOrderPoint4Arrows "

|;
;;命名必须是唯一的

;| 管线命令 集合 |;
(defun c:cg (/ dclst)
  (setq dclst '(("绘制"
                 ("0.初始化 Init "
                  "1.导入坐标 FetchPointIntoDwg "
                  "2.删除重复 EraseRepeatData"
                  "设置工作图层 SetCurrentLayer"
                  "3.方向(流向)(&D) MakeArrows "
                  "4.提取高程 AddBlockH "
		  "5.设置节点属性 SetBlockNodeAttr"
		  "6.生成节点序号 GetOrderId"
                  "7.合并ID块 CombineBlockId"
                  "D.数据输出 ExportDataNodeArrow "
                 )
                )

                ("管径数据"
                 ("ID-设置管径编号 SetArrowId"
                  "ID-插入管线编号 InsertArrowId"
                  "ID-显示管径ID ShowArrowId"
                  "ID-隐藏管径ID EraseArrowId"
		   "管径数据 ~002 "
                  "D-设置管径 SetArrowData"
                  "D-重置管径 ResetArrowId"
                  "D-显示管径 ShowArrowText"
                 )
                )

;;;                ("B 数据"
;;;                 ( ;"检查 ~JC"
;;;                  "C.核查_节点块 CheckBlockId "
;;;                  "C.核查_箭头 CheckArrow "
;;;		  "C.箭头长度 CheckArrowLen"
;;;                  "查看_管径 ShowXDataDis "
;;;                  "节点插入块 InsertPointBlock"
;;;                  "文档整理 ~12 "
;;;                  "1.Oh->文本 Oh->Text"
;;;                  "2.将箭头->多段线 Arrows2Pline "
;;;                  "3.隐藏辅助数据 layerFreeze_080 "
;;;                 )
;;;                )

;;;                ("辅助"
;;;                 ("1.计算线段长度 SumCurveLen "
;;;                  "2.检测多段线长度 CheckPlineLen "
;;;                  "3.获取块点 GetPointFromBlockElevation"
;;;                 )
;;;                )
               )
  )
  (while
    (Dcl-ButtonM "「管线工具箱」v1" "功能说明\n根据需要设计" dclst nil)
  )
  (princ)
)


 ;| 工具箱 集合 |;
(defun c:cgt (/ dcllst)
  (setq dcllst '(
                 ("A 高程相关"
                  ("1.绘制矢量箭头 MakeArrows "
                   "2.提取高程 AddBlockOh "
                   "3.插入Oh块(&D) InsertBlock_oh "
                   "4.箭头->序号 MarkOrderPoint4Arrows "
                   "4.Pl->序号 MarkOrderPoint4Plines "
                   "5.Oh->ID合并 CombineBlockAuto->oh "
                   "6.管径->箭头 PutXDataText->Pline "
                   "7.箭头->管径 MarkArrow->Dis "
;;;					"块 ~k "
;;;					".创建_0h_图块 CreateBlack_oh "
                  )
                 )
                 ("B 数据"
                  ( ;"检查 ~JC"
                   "C.核查_Oh块 CheckOh "
                   "C.核查_箭头 CheckArrowOh "
                   "查看_管径 ShowXDataDis "
                   "D.数据输出 ohArrow_Cad2txt "
                   ""
                   "文档整理 ~12 "
                   "1.Oh->文本 Oh->Text"
                   "2.将箭头->多段线 Arrows2Pline "
                   "3.隐藏辅助数据 layerFreeze_080 "
                  )
                 )
                 ("操作"
                  ("1.合并TXT断面 hbtxt "
                   "2.合并直线与多段线 PlineJoinline "
                   "3.合并多段线 PlineJoin "
                   "4.打断多段线 PlineAutoBreak "
                   "5.反转多段线 RevPline "
                   "6. ~1 "
                  )
                 )
                 ("计算"
                  (
                   "1.计算线段长度 SumCurveLen "
                   "2.检测多段线长度 CheckPlineLen "
                   "4.计算高程 CalculateHight "
                   "0.不启用 ~MakeArrows "
                  )
                 )
                )
  )
  (setq strHelp (strcat
                  "操作步骤\n"     "\n1.绘制流向"   "\n2.提取高程点"
                  "\n3."           "\n4."           "\n5."
                 )
  )
  (while (Dcl-ButtonM "「管线工具箱」v1" strHelp dcllst nil))
  (princ)
)

 ;|Dcl-ButtonM
__
	titl标题
	hltxt功能说明
	buttons按钮列表
	flag非真时先行后列T先列后行
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
            "{spacer_0;:button{key = \"HLP\";label = \"帮助\";}"
            ":button{key=\"ESC\";label=\"取消(&Q)\";is_cancel = true;}}}} "
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
              "没有功能说明"
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
            (t (alert (strcat "功能函数" a "不存在")))
      )
      t
    )
  )
)

