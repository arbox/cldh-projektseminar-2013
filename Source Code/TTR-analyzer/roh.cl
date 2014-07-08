;;; Code for the form named :form1 of class dialog.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)


;;;Liste mit 3-er-Listen 
;;;1. Ranking, 2. TTR, 3. Token-Nr.
;;;geordnet nach Ranking
(defun rank(liste)
  (let ((ranking 1)
        (same 0)
        (l ())
        (erg ())
        (ende ()))
  (loop
    for item in liste 
    do
    (if (equal (car item) (car (nth (+ (position item liste) 1) liste )))
        (progn
          (setf same (+ same ranking))
          (setf l (append l (list item)))
          )
      (progn
        (if (> same 0)
            (progn
              (setf l (append l (list item)))
              (setf erg (append erg (list (cons ( / (+ same ranking) (length l)) l))))
              (setf l () same 0)
            )
            (setf erg (append erg (list(cons ranking item))))))
        )
    (incf ranking)
    )
    (loop
      for item in erg
      do
      (if (atom (second item))
          (setf ende (adjoin item ende))
        (loop
          for i in item
          do
          (unless (atom i)
            (setf ende (adjoin (cons (first item) i) ende))))))
          (sort ende #'< :key #'third)
    ))

;;;Assoziationsliste(?nope) mit ranking für den zweiten text(über token-nr.)
;;; rankings subtrahieren,quadireren, aufaddieren etc.

(defun set_ds(x y)
  (apply '+
         (mapcar #'(lambda (x y) (expt (- (first x) (first y)) 2))
          x
          y))
  )

(defun roh_formula (x y)
  (- 1 (/ (* 6 (set_ds x y)) (* (length x) (- (expt (length x) 2) 1))))
  )

(defun compute_roh(textx texty)
  (let ((p 0)
        (lx textx)
        (ly texty))
 
       (setf 
        lx (rank (sort (mapcar #'(lambda (x) (incf p) (list x p)) (Text-ttr_list lx)) #'> :key #'car))
        ly (rank (sort (mapcar #'(lambda (x) (incf p) (list x p)) (Text-ttr_list ly)) #'> :key #'car)))

    (float(roh_formula lx ly))))
