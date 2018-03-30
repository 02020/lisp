
 
  ;������ͼ��
(defun c:df ()
  (princ "\no_o ������ͼ��")
  (vlex-LayerOn (vlex-ListLayers))
  (princ)
)

  ;����ѡ���ͼ��
(defun c:dd ()
  (SETVAR "CMDECHO" 0)
  (setvar "EXPERT" 0)
  (princ "\no_o ����ѡ���ͼ��")

  (setq
    ss        (ssget)
    layersAll (vlex-ListLayers)
    layers    (e-GetlayersFromEn ss)
    layers    (list-subtract layersAll layers)
  )
  (if (/= layers nil)
    (vlex-LayerOff layers)
  )
  (princ)
)


  ;�ر�ѡ���ͼ��
(defun c:dg ()
  (SETVAR "CMDECHO" 0)
  (setvar "EXPERT" 0)
  (princ "\no_o �ر�ѡ���ͼ��")

  (setq ss (ssget))
  (setq
    layers (e-GetlayersFromEn ss)
  )
  (if (/= layers nil)
    (progn
      (setq layer (apply 'strcat
                         (mapcar '(lambda (x) (strcat x ",")) layers)
                  )
      )
      (princ (strcat "\no_o �رյ�ͼ�㣺" layer))
      (vlex-LayerOff layers)
    )

  )
  (princ)
)


  ;����ѡ���ͼ��
(defun c:dj ()
  (princ "\no_o ����ѡ���ͼ��")
  (setq ss (ssget))
  (setq
    layers (e-GetlayersFromEn ss)
  )
  (if (/= layers nil)
    (progn
      (setq layer (apply 'strcat
                         (mapcar '(lambda (x) (strcat x ",")) layers)
                  )
      )
      (princ (strcat "\no_o �����ͼ�㣺" layer))
      (vlex-LayerFreeze layers)
    )

  )
  (princ)
)

 ;�ر�ģ�������ͼ��
(defun c:dgb ()
  (princ "\no_o �ر�ģ�������ͼ��")
  (vlex-LayerOff
    '("1�߻�" "2��Ԫ������" "3Ȩ����" "4����ע��" "5˵��ע��")
  )
  (princ)
)