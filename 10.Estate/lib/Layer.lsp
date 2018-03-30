;;Ñ¡Ôñ¼¯
(defun e-GetlayersFromEn (ss / layers layer n1)
  (setq
    n1 -1
    layers nil
  )
  (if (not (null ss))
    (while (setq s1 (ssname ss (setq n1 (1+ n1))))
      (setq layer  (cdr (assoc '8 (entget s1)))
            layers (cons layer layers)
      )
    )

  )
  layers
)
