
 
  ;打开所有图层
(defun c:df ()
  (princ "\no_o 打开所有图层")
  (vlex-LayerOn (vlex-ListLayers))
  (princ)
)

  ;保留选择的图层
(defun c:dd ()
  (SETVAR "CMDECHO" 0)
  (setvar "EXPERT" 0)
  (princ "\no_o 保留选择的图层")

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


  ;关闭选择的图层
(defun c:dg ()
  (SETVAR "CMDECHO" 0)
  (setvar "EXPERT" 0)
  (princ "\no_o 关闭选择的图层")

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
      (princ (strcat "\no_o 关闭的图层：" layer))
      (vlex-LayerOff layers)
    )

  )
  (princ)
)


  ;冻结选择的图层
(defun c:dj ()
  (princ "\no_o 冻结选择的图层")
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
      (princ (strcat "\no_o 冻结的图层：" layer))
      (vlex-LayerFreeze layers)
    )

  )
  (princ)
)

 ;关闭模板的所有图层
(defun c:dgb ()
  (princ "\no_o 关闭模板的所有图层")
  (vlex-LayerOff
    '("1线划" "2单元及代码" "3权属线" "4数据注记" "5说明注记")
  )
  (princ)
)