
(in-package :common-graphics-user)

;;;Generiert einen Graph des TTR-Verlaufs
;;;n=chart widget
;;;x=text structure
(defun plot-TTR(n x)
  (let
     ((ii 1) (TTR_Liste (Text-ttr_list x)))
      (loop
     for item in (Text-tokenized x)
      do
        (set-chart-value n :object-index  (- (Text-id x) 1) :item-index ii  :value (nth (- ii 1) TTR_Liste))
        (setf ii (+ 1 ii)))
    )
  )

;;;Graph des KGM
(defun plot-KGM(n x)
  (let
     ((ii 1) (text (Text-tokenized x)) (kgm_liste (Text-kgm x)))
      (loop
     for item in text
      do
        (set-chart-value n :object-index (- (Text-id x) 1) :item-index ii  :value (nth (- ii 1) kgm_liste))
        (setf ii (+ 1 ii)))
    )
  )