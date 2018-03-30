;|

���ߤ��ߵ����Բ���


 |;

 
;| ���ݸ̵߳���� ʸ����ͷ |;
(defun MakeArrows (/ pt1 pt2 pt3 l)
  (setvar "pickbox" 16)
  (princ "\n���� ʸ����ͷ ���Զ���")
  (setq blockName "0-Elevation")
  (setvar "osmode" 4) ;Բ��
  (setq id (GetArrowId))

  (if (setq pt1 (GetPointFromAttr blockName))
    (while (setq pt2 (GetPointFromAttr blockName))
      (setq pt2 (list (car pt2) (cadr pt2)))
      (if (> (distance pt1 pt2) 0.0001)
	(progn
	  (if (< (distance pt1 pt2) 5)
	    (setq l (* (distance pt1 pt2) 0.5))
	    (setq l 2)
	  )
	  (setq pt3 (polar pt2 (angle pt2 pt1) l))
	  (setq lstpt (list (list pt1 0.1) (list pt3 0.6 0.05) (list pt2)))
	  (setq	en     (#EntMake_pline_k (list lstpt "080_A"))
		o (vlax-ename->vla-object en)
	  )
	  (ex:PutXData o (list (cons 1001 "id") (cons 1000 id)))
	  (setq id (1+ id))
	  (setq pt1 pt2)
	)
      )
    )
  )
  (setvar "osmode" 33)
  (princ)
)




;| �����������xData����ֵ |;
(defun SetArrowData ()
  (defun XData (o key value)
    (ex:PutXData o (list (cons 1001 key) (cons 1000 value)))
  )
  (setq
    lx	     (getstring "\no__o ��������: ")
    diameter (getstring "\no__o ����ܾ�: ")
    hole     (getstring "\no__o ����׸���: ")
    inbuilt  (getstring "\no__o �������跽ʽ: ")
  )
  (princ "\no__o ѡ���ͷ")
  (setq ssPl (ssget (list (cons 0 "LWPOLYLINE") (cons 8 "080_A"))))
  (setq n1 -1)
  (while (setq en (ssname ssPl (setq n1 (+ n1 1))))
    (setq o   (vlax-ename->vla-object en)
	  len (vlax-curve-getdistatparam
		o
		(vlax-curve-getendparam o)
	      )
    )
    (XData o "lx" lx)
    (XData o "diameter" diameter)
    (XData o "hole" hole)
    (XData o "inbuilt" inbuilt)
    (XData o "length" (rtos len 2 2))
  )
)



;;��ȡArrowId
(defun GetArrowId (/ maxId n1)
  (setq
    maxId (1+ (GetArrowMaxId))
    n1    (getreal (strcat "\no__o ������ʼ���<" (itoa maxId) ">:"))
  )
  (if (null n1)
    (setq n1 maxId)
  )
  n1
)



;;���ù��ߵ�id
(defun SetArrowId (/ ssArrow fontSize ptm vlaObj id)
  (setq fontSize 0.2)
  (setvar "clayer" "080_A")
  (princ "\no__o ���ü�ͷ�ı��")
  (setq n1 (GetArrowId))
  (while (setq ssArrow
                (ssget ":s" (list (cons 0 "LWPOLYLINE") (cons 8 "080_A")))
         )
    (setq
      en     (ssname ssArrow 0)
      vlaObj (vlax-ename->vla-object en)
      ptm    (GetEntityMiddle en)
    )
    (entmake (list '(0 . "TEXT")
                   (cons 1 (rtos n1))
                   (cons 10 (car ptm))
                   (cons 40 fontSize)
                   (cons 39 47)
                   (cons 50 (cadr ptm))
             )
    )
    (ex:PutXData vlaObj (list (cons 1001 "id") (cons 1000 n1)))
    (setq n1 (1+ n1))
  )
)

;;������ߵ�id
(defun InsertArrowId (/ ssArrow fontSize ptm vlaObj id)
  (setq fontSize 0.2)
  (setvar "clayer" "080_A")
  (princ "\no__o �����ͷ�ı��")
  (setq
    id (getreal (strcat "\no__o ������"))
  )
  (if (null id)
    (exit)
  )
  (setq ssArrowAll
         (ssget "x" (list (cons 0 "LWPOLYLINE") (cons 8 "080_A")))
  )
  (setq
    ssArrow (ssget ":s" (list (cons 0 "LWPOLYLINE") (cons 8 "080_A")))
  )
  (setq
    en         (ssname ssArrow 0)
    currentObj (vlax-ename->vla-object en)
  )
  (mapcar
    '(lambda (x)
       (setq en     (cadr x)
             xdata  (assoc -3 (entget en '("id")))
             vlaObj (vlax-ename->vla-object en)
       )
       (if xdata
         (progn
           (setq _id (atoi (cdr (cadadr xdata))))

           (if (>= _id id)
             (ex:PutXData
               vlaObj
               (list (cons 1001 "id") (cons 1000 (1+ _id)))
             )
           )

         )
       )
     )
    (ssnameX ssArrowAll)
  )
  (ex:PutXData currentObj (list (cons 1001 "id") (cons 1000 (rtos id))))

  (ShowArrowId)
)

;;������ߵ�id
(defun ResetArrowId (/ ssArrow fontSize ptm vlaObj id)
  (setq
    ssArrow (ssget "x" (list (cons 0 "LWPOLYLINE") (cons 8 "080_A")))
  )
  (setq arrowList (mapcar 'GetArrowIdFromX (ssnameX ssArrow))
        arrowList (vl-sort arrowList
                           (function (lambda (e1 e2) (< (car e1) (car e2))))
                  )
  )
  (setq n1 0)
  (foreach en arrowList
    (setq vlaObj (vlax-ename->vla-object (cadr en))
          n1     (1+ n1)
    )
    (ex:PutXData vlaObj (list (cons 1001 "id") (cons 1000 n1)))
  )
  (ShowArrowId)
  (princ (strcat "\no__o "
                 (rtos n1)
                 " "
                 (rtos (sslength ssArrow))
         )
  )
)


;;���Ʊ��
(defun GetOrderId ()
  (princ "\no__o ���ɽڵ����")
     (DelAllBolck "0-Id" "080_A")
  (setq lstPt nil
    ssArrow (ssget "x" (list (cons 0 "LWPOLYLINE") (cons 8 "080_A")))
  )
  (setq arrowList (mapcar 'GetArrowIdFromX (ssnameX ssArrow))
        arrowList (vl-sort arrowList
                           (function (lambda (e1 e2) (< (car e1) (car e2))))
                  )
  )
  (foreach en arrowList
    (setq
      id     (car en)
      vlaObj (vlax-ename->vla-object (cadr en))
      pte    (vlax-curve-getEndPoint vlaObj)
      pts    (vlax-curve-getStartPoint vlaObj)

    )
    (setq lstPt (cons pts lstPt))
    (setq lstPt (cons pte lstPt))
  )
  ;��ת����������ٽ����޳��ظ�
  (setq lstPt    (reverse lstPt)
        nodeList (GetUniquePointFromList lstPt)
  )
  (DelAllBolck "0-Id" "080_A")
  (ShowBlock_Oh nodeList "0.2")
)



;;ɾ���ܾ�ID
(defun EraseArrowId ()
  (setq ssArrowId
         (ssget "x"
                (list (cons 0 "TEXT") (cons 39 47) (cons 8 "080_A"))
         )
  )
 (if ssArrowId
  (vl-cmdf "ERASE" ssArrowId ""))
)

;;��ʾ�ܾ�ID
(defun ShowArrowId (/ ssArrow n1 en ptm vlaObj)
  (setq fontSize 0.2)
  (setq
    ssArrow (ssget "x" (list (cons 0 "LWPOLYLINE") (cons 8 "080_A")))
  )

  (EraseArrowId)
  (setq n1 -1)
  (while (setq en (ssname ssArrow (setq n1 (+ n1 1))))
    (setq vlaObj (vlax-ename->vla-object en)
          ptm    (GetEntityMiddle en)
          xdata  (assoc -3 (entget en '("id")))
    )
    (setq id (if xdata
               (atoi (cdr (cadadr xdata)))
               0
             )
    )
    (entmake (list '(0 . "TEXT")
                   (cons 1 (rtos id))
                   (cons 10 (car ptm))
                   (cons 40 fontSize)
                   (cons 39 47)
                   (cons 50 (cadr ptm))
             )
    )
  )
  (vl-cmdf "undo" "e")
)


;;��ȡid
(defun GetArrowIdFromX (ssX / en xdata)
  (setq
    en    (cadr ssX)
    xdata (assoc -3 (entget en '("id")))
  )
  (setq id (if xdata
             (atoi (cdr (cadadr xdata)))
             0
           )
  )
  (list id en)
)



;;��ȡ����id
(defun GetArrowMaxId (/)
  (setq
    ssArrow (ssget "x" (list (cons 0 "LWPOLYLINE") (cons 8 "080_A")))
  )
  (if (null ssArrow)
    0
    (progn
      (setq arrowList (mapcar 'GetArrowIdFromX (ssnameX ssArrow)))
      (apply 'max (mapcar 'car arrowList))
    )
  )
)

;;�Զ����ù���id___
(defun SetAllArrowId (/ ssArrow fontSize ptm vlaObj id)
  (setq fontSize 0.2)
  (princ "\no__o �������м�ͷ�ı��")
  (setq
    ssArrow (ssget "x" (list (cons 0 "LWPOLYLINE") (cons 8 "080_A")))
  )
  (setq n1 -1)
  (while (setq en (ssname ssArrow (setq n1 (+ n1 1))))
    (setq vlaObj (vlax-ename->vla-object en)
          ptm    (GetEntityMiddle en)
          id     (strcat "" (rtos n1))
    )
    (#EntMake_Text id (car ptm) fontSize (cadr ptm))
    (ex:PutXData vlaObj (list (cons 1001 "id") (cons 1000 (1+ n1))))
  )
  (princ
    (strcat "\no__o ��ȡ���� ʸ����ͷ ����Ϊ:"
            (itoa (sslength ssArrow))
    )
  )
  (princ)
)


 ;| ��ʾ�ܾ����� |;
(defun ShowArrowText ()
  (setq fontSize configDiameterFontSize
        keyList  (list  "lx" "diameter" "hole" "inbuilt")
  )
  (setvar "pickbox" 20)
  (setvar "clayer" "080_A")
  (while (setq en (entsel "\no__o ��ѡѡ�� ��ͷ"))
    (setq
      ptA  (GetEntityMiddlePosition en fontSize)
      text (GetXData->diameter (car en) keyList " ")
    )
    (if text
      (#EntMake_Text text (car ptA) fontSize (cadr ptA))
      (princ "\no__o �ü�ͷδ��������")
    )
  )
  (setvar "pickbox" 10)
)

;;��ȡͼ�ζ�����е�
(defun GetEntityMiddle (en / plObj pt1 pt2 ang ptd)
  (setq plObj (vlax-ename->vla-object en)
        pt1   (vlax-curve-getStartPoint plObj)
        pt2   (vlax-curve-getEndPoint plObj)
        ang   (angle pt1 pt2)
        ptd   (mapcar '(lambda (x) (/ x 2)) (mapcar '+ pt1 pt2))
  )
  (list ptd (+ (* pi 1) ang))
)



;;��ȡͼ�ζ�����е�
(defun GetEntityMiddlePosition (en fontSize)
  (setq offset 0.0) ;���ñ��˾���
  (setq plObj (vlax-ename->vla-object (car en))
        pt1   (vlax-curve-getStartPoint plObj)
        pt2   (vlax-curve-getEndPoint plObj)
        ang   (angle pt1 pt2)
        ptd   (mapcar '(lambda (x) (/ x 2)) (mapcar '+ pt1 pt2))
        pts   (cadr en)
        angd  (angle ptd pts)
  )
  (if (> (- ang angd) 0)
    (setq an 1.5
          h  offset
    )
    (setq an 0.5
          h  (+ offset fontSize)
    )
  )
  (setq
    ptd (polar ptd (+ ang (* pi an)) 1)
  )
  (if (< (car pt1) (car pt2))
    (progn
      (if (> (- ang angd) 0)
        (setq h (+ offset fontSize))
        (setq h offset)
      )
      (setq angt (+ pi ang))
      (setq ptt (polar ptd (+ ang (* pi 0)) -1.5))
      (setq ptt (polar ptt (+ ang (* pi an)) h))
    )
    (progn
      (setq angt ang)
      (setq ptt (polar ptd (+ ang (* pi 0)) 1.5))
      (setq ptt (polar ptt (+ ang (* pi an)) h))
    )
  )
  (list ptt (+ (* pi 1) angt))
)

;;��ȡ��������
(defun GetXData->diameter (en keyList dem)
  (setq vlaObj (vlax-ename->vla-object en))
  (if (assoc -3 (entget en keyList))
    (apply
      'strcat
      (mapcar '(lambda (x) (strcat (GetXData vlaObj x) dem)) keyList)
    )
    nil
  )
)



(defun GetXData (vlaObj key)
  (cdr (assoc 1000 (ex:GetXData vlaObj key)))
)

