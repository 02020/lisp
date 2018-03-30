;;; -------------------------------------------------------------------------
;;; �ı�����ˢV3.0  ����fa     --by ���� 2016.04
;;; ֧�ֿ������֣���������ֻ�ܵ�ѡ���������ֿ��Կ�ѡ�����������֡��������֡��������֡�����ͼ����������ߡ��������֡�������������
(vl-load-com)
(setq source_text nil) ; ��Դ����Ϊȫ�ֱ���
(defun c:fa (/ en en_data en1 en1_data ent entype entype_source i ob pt ss ss_data txtst *error* x)
  (defun *error* (x) ;������
      (if en (redraw (car en) 4))
      (setvar "ErrNo" 0)
      (setvar "cmdecho" 1)
  )
  (setvar "cmdecho" 0)
  (setvar "ErrNo" 0)
  (if (= source_text nil)
    (if (setq en (nentsel "\n��ѡ��Դ����(�Ҽ��˳�):"))
    (progn
      (setq en_data (entget (car en)))
      (setq entype_source (cdr (assoc 0 en_data)))
      (if (or (setq txtst (cdr (assoc 7 en_data))) (= entype_source "TCH_DRAWINGNAME"))
       (progn
         (redraw (car en) 3)
         (if (= entype_source "ATTDEF") ;����������֣���ȡ����ǡ�ΪԴ����
           (setq source_text (cdr (assoc 2 en_data)))
           (setq source_text (cdr (assoc 1 en_data)))
         )
      ))
     )
    )
    (if (and (= (setq en (nentsel (strcat "\n��ѡ��Դ����: Ĭ��:" source_text))) nil) (= (getvar "ErrNo") 52))
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
             (if (= entype_source "ATTDEF") ;����������֣���ȡ����ǡ�ΪԴ����
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
  (prompt "\n��ѡ��Ҫ�޸����ݵ�����:")
  (while (/= (getvar "ErrNo") 52)
    (prompt (strcat "\n�������ݽ���ˢ��:" source_text))
    (if (and (setq ss (ssget ":S" '((0 . "*TEXT,TCH_DRAWINGNAME,TCH_ELEVATION,INSERT,ATTDEF,ATTRIB")))) source_text)
      (progn
	(if (= (caar (setq ss_data (ssnamex ss 0))) 1)
	  (progn		       ; ��ѡʱ
	    (setq ent (ssname ss 0)
		  pt (trans (cadr (last (car ss_data))) 0 1)
		  en1 (car (nentselp pt))
		  en1_data (entget en1)
		  entype (cdr (assoc 0 en1_data))
		  ob (vlax-ename->vla-object en1)
	    )
	    (wenzishua entype entype_source ob source_text en1 ent)
	  )
	  (progn		       ; ��ѡʱ
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

;����ˢ�ӳ���
(defun wenzishua (entype entype_source ob source_text en1 ent) 
  ; cad��������
  (if (= entype "MTEXT")
    (progn
      (vla-put-TextString ob source_text)
      (entupd en1)
      (entupd ent)
    )
  )
  ;ȥ�������������ø�ʽ����
  (if (= entype_source "MTEXT")
    (setq source_text (mtext2text source_text))
  )
  ; cad��������
  (if (= entype "TEXT")
    (progn
      (vla-put-TextString ob source_text)
      (entupd en1)
      (entupd ent)
    )
  )
  ; �������ֵ����ݸ�ʽˢ
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
  ; ����ͼ������ߵ����ݸ�ʽˢ
  (if (= entype "TCH_DRAWINGNAME")
    (progn
      (vlax-put-property ob 'NameText source_text)
      (entupd en1)
      (entupd ent)
    )
  )
  ; �������� ֻ��"���"
  (if (= entype "ATTDEF")
    (progn
      (vla-put-TagString ob source_text);�ı��
      (entupd en1)
      (entupd ent)
    )
  )
  ; ������������ ֻ��"Ĭ��"
  (if (= entype "ATTRIB")
    (progn
      (vla-put-TextString ob source_text);��Ĭ��
      (entupd en1)
      (entupd ent)
    )
  )
)

;��ȡ��������,ȥ�����ø�ʽ����--��������
(defun mtext2text(MTextString / regex s)
  (setq regex(vlax-create-object "Vbscript.RegExp")) ;����������ʽ�ؼ�
  (vlax-put-property regex "IgnoreCase" 0) ;�����Դ�Сд
  (vlax-put-property regex "Global" 1) ;ƥ�䷽ʽ��ȫ����ƥ��
  (setq s MTextString)
     ;�滻\\�ַ�
  (vlax-put-property regex "Pattern" "\\\\\\\\")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 1)))
     ;�滻\{�ַ�
  (vlax-put-property regex "Pattern" "\\\\{")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 2)))
     ;�滻\}�ַ�
  (vlax-put-property regex "Pattern" "\\\\}")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 3)))
     ;ɾ������������ʽ
  (vlax-put-property regex "Pattern" "\\\\pi(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ���Ʊ����ʽ
  (vlax-put-property regex "Pattern" "\\\\pt(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ���ѵ���ʽ
  (vlax-put-property regex "Pattern" "\\\\S(.[^;]*)(\\^|#|\\\\)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ�����塢��ɫ���ָߡ��־ࡢ��б���ֿ������ʽ
  (vlax-put-property regex "Pattern" "(\\\\F|\\\\f|\\\\C|\\\\H|\\\\\T|\\\\Q|\\\\W|\\\\A)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ���»��ߡ�ɾ���߸�ʽ
  (vlax-put-property regex "Pattern" "(\\\\L|\\\\O|\\\\l|\\\\o)")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ������Ͽո��ʽ
  (vlax-put-property regex "Pattern" "\\\\~")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ�����з���ʽ
  (vlax-put-property regex "Pattern" "\\\\P")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ�����з���ʽ(���Shift+Enter��ʽ)
  (vlax-put-property regex "Pattern" "\n")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ��{}
  (vlax-put-property regex "Pattern" "({|})")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     
     ;�滻��\\,\{,\}�ַ�
  (vlax-put-property regex "Pattern" "\\x01")
  (setq s(vlax-invoke-method  regex "Replace" s "\\"))
  (vlax-put-property regex "Pattern" "\\x02")
  (setq s(vlax-invoke-method  regex "Replace" s "{"))
  (vlax-put-property regex "Pattern" "\\x03")
  (setq s(vlax-invoke-method  regex "Replace" s "}"))
     
  (vlax-release-object regex)
  s
)