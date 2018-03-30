;;; -------------------------------------------------------------------------
;;; 文本内容刷V3.0  命令fa     --by 阿甘 2016.04
;;; 支持块中文字（块中文字只能点选，其它文字可以框选）、单行文字、多行文字、天正文字、天正图名、天正标高、属性文字、块中属性文字
(vl-load-com)
(setq source_text nil) ; 设源文字为全局变量
(defun c:fa (/ en en_data en1 en1_data ent entype entype_source i ob pt ss ss_data txtst *error* x)
  (defun *error* (x) ;出错函数
      (if en (redraw (car en) 4))
      (setvar "ErrNo" 0)
      (setvar "cmdecho" 1)
  )
  (setvar "cmdecho" 0)
  (setvar "ErrNo" 0)
  (if (= source_text nil)
    (if (setq en (nentsel "\n请选择源文字(右键退出):"))
    (progn
      (setq en_data (entget (car en)))
      (setq entype_source (cdr (assoc 0 en_data)))
      (if (or (setq txtst (cdr (assoc 7 en_data))) (= entype_source "TCH_DRAWINGNAME"))
       (progn
         (redraw (car en) 3)
         (if (= entype_source "ATTDEF") ;如果是属性字，则取“标记”为源文字
           (setq source_text (cdr (assoc 2 en_data)))
           (setq source_text (cdr (assoc 1 en_data)))
         )
      ))
     )
    )
    (if (and (= (setq en (nentsel (strcat "\n请选择源文字: 默认:" source_text))) nil) (= (getvar "ErrNo") 52))
      (progn
       (setvar "ErrNo" 0)
       (setq txtst T)
      )
      (if en
       (progn
         (setq en_data (entget (car en)))
         (setq entype_source (cdr (assoc 0 en_data)))
         (if (or (setq txtst (cdr (assoc 7 en_data))) (= entype_source "TCH_DRAWINGNAME"))
           (progn
             (redraw (car en) 3)
             (if (= entype_source "ATTDEF") ;如果是属性字，则取“标记”为源文字
               (setq source_text (cdr (assoc 2 en_data)))
               (setq source_text (cdr (assoc 1 en_data)))
            )
         ))
	   )
	   (setvar "ErrNo" 52)
     )
    )
  )
  (if (or txtst (= entype_source "TCH_DRAWINGNAME"))
  (progn
  (prompt "\n请选择要修改内容的文字:")
  (while (/= (getvar "ErrNo") 52)
    (prompt (strcat "\n文字内容将被刷成:" source_text))
    (if (and (setq ss (ssget ":S" '((0 . "*TEXT,TCH_DRAWINGNAME,TCH_ELEVATION,INSERT,ATTDEF,ATTRIB")))) source_text)
      (progn
	(if (= (caar (setq ss_data (ssnamex ss 0))) 1)
	  (progn		       ; 点选时
	    (setq ent (ssname ss 0)
		  pt (trans (cadr (last (car ss_data))) 0 1)
		  en1 (car (nentselp pt))
		  en1_data (entget en1)
		  entype (cdr (assoc 0 en1_data))
		  ob (vlax-ename->vla-object en1)
	    )
	    (wenzishua entype entype_source ob source_text en1 ent)
	  )
	  (progn		       ; 框选时
	    (setq i 0)
	    (repeat (sslength ss)
	      (setq en1 (ssname ss i)
		    ent en1
		    en1_data (entget en1)
		    entype (cdr (assoc 0 en1_data))
		    ob (vlax-ename->vla-object en1)
	      )
	      (wenzishua entype entype_source ob source_text en1 ent)
	      (setq i (1+ i))
	    )			       ; end repeat
	  )
	)
      )
    )
  ); end while
  ))
  (if en (redraw (car en) 4))
  (setvar "ErrNo" 0)
  (setvar "cmdecho" 1)
  (princ)
)

;文字刷子程序
(defun wenzishua (entype entype_source ob source_text en1 ent) 
  ; cad多行文字
  (if (= entype "MTEXT")
    (progn
      (vla-put-TextString ob source_text)
      (entupd en1)
      (entupd ent)
    )
  )
  ;去掉多行文字无用格式符号
  (if (= entype_source "MTEXT")
    (setq source_text (mtext2text source_text))
  )
  ; cad单行文字
  (if (= entype "TEXT")
    (progn
      (vla-put-TextString ob source_text)
      (entupd en1)
      (entupd ent)
    )
  )
  ; 天正文字的内容格式刷
  (if (or
	(= entype "TCH_TEXT")
	(= entype "TCH_ELEVATION")
      )
    (progn
      (vlax-put-property ob 'Text source_text)
      (entupd en1)
      (entupd ent)
    )
  )   
  ; 天正图名、标高的内容格式刷
  (if (= entype "TCH_DRAWINGNAME")
    (progn
      (vlax-put-property ob 'NameText source_text)
      (entupd en1)
      (entupd ent)
    )
  )
  ; 属性文字 只改"标记"
  (if (= entype "ATTDEF")
    (progn
      (vla-put-TagString ob source_text);改标记
      (entupd en1)
      (entupd ent)
    )
  )
  ; 块中属性文字 只改"默认"
  (if (= entype "ATTRIB")
    (progn
      (vla-put-TextString ob source_text);改默认
      (entupd en1)
      (entupd ent)
    )
  )
)

;提取多行文字,去除无用格式符号--来自明经
(defun mtext2text(MTextString / regex s)
  (setq regex(vlax-create-object "Vbscript.RegExp")) ;引用正则表达式控件
  (vlax-put-property regex "IgnoreCase" 0) ;不忽略大小写
  (vlax-put-property regex "Global" 1) ;匹配方式，全文字匹配
  (setq s MTextString)
     ;替换\\字符
  (vlax-put-property regex "Pattern" "\\\\\\\\")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 1)))
     ;替换\{字符
  (vlax-put-property regex "Pattern" "\\\\{")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 2)))
     ;替换\}字符
  (vlax-put-property regex "Pattern" "\\\\}")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 3)))
     ;删除段落缩进格式
  (vlax-put-property regex "Pattern" "\\\\pi(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除制表符格式
  (vlax-put-property regex "Pattern" "\\\\pt(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除堆迭格式
  (vlax-put-property regex "Pattern" "\\\\S(.[^;]*)(\\^|#|\\\\)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除字体、颜色、字高、字距、倾斜、字宽、对齐格式
  (vlax-put-property regex "Pattern" "(\\\\F|\\\\f|\\\\C|\\\\H|\\\\\T|\\\\Q|\\\\W|\\\\A)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除下划线、删除线格式
  (vlax-put-property regex "Pattern" "(\\\\L|\\\\O|\\\\l|\\\\o)")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除不间断空格格式
  (vlax-put-property regex "Pattern" "\\\\~")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除换行符格式
  (vlax-put-property regex "Pattern" "\\\\P")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除换行符格式(针对Shift+Enter格式)
  (vlax-put-property regex "Pattern" "\n")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除{}
  (vlax-put-property regex "Pattern" "({|})")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     
     ;替换回\\,\{,\}字符
  (vlax-put-property regex "Pattern" "\\x01")
  (setq s(vlax-invoke-method  regex "Replace" s "\\"))
  (vlax-put-property regex "Pattern" "\\x02")
  (setq s(vlax-invoke-method  regex "Replace" s "{"))
  (vlax-put-property regex "Pattern" "\\x03")
  (setq s(vlax-invoke-method  regex "Replace" s "}"))
     
  (vlax-release-object regex)
  s
)