(in-package :common-graphics-user)

;;;Naiver Tokenizer ohne Ber�cksichtigung von Lemmata.
;;;Trennt W�rter anhand von Leerzeichen und anderen Sonderzeichen
;;;Nimmt einen String x und gibt eine Liste mit allen Tokens aus
(defun naive_tokenizer(x)
    (let
    ((s "") l)  
    (loop
    for item across x
      do (cond
          ((or (char= item #\Space) (char= item #\.) 
               (char= item #\!) (char= item #\?) 
               (char= item #\:) (char= item #\,) 
               (char= item #\;) (char= item #\")
               (char= item #\Newline) (char= item #\Tab))
           (unless (equal s "") (prog1 (setq l (append l (list (string-downcase s)))) (setq s ""))))
          (t
           (setq s (concatenate 'string s (string item)))))
      )
      (unless (equal s "") (prog1 (setq l (append l (list (string-downcase s)))) (setq s "")))
      l)
  )
              
;;;Erh�lt eine Liste x mit Tokens und z�hlt alle darin vorkommende Types
(defun typezahl(x)
  (let
      ((type '()))
  (loop
   for item in x
    do 
    (unless (find item type :test #'equal) (setf type (append type (list item))))
    )
   (list-length type))
  ) 

;;;Erh�lt eine Liste x Tokens und errechnet die TTR indem es die Anzahl der Types durch Anzahl der Tokens dividiert
(defun simple_TTR(x)
  (if (= (length x) 0)
      1
      (float(/ (typezahl x) (length x)))))

;;;Erh�lt eine Liste x Tokens und gibt zu jedem Token die TTR aus
(defun list_TTR(x)
  (let ((count 1)
        l)
  (loop
     for item in x
     do
        (setq l (append l (list (simple_TTR (subseq x 0 count)))))
        (incf count)
    )
    l)
  )

;;;Erh�lt eine Liste x mit Tokens und errechent den Measure of Textual Diversity von McCarthy
;;;
(defun mtld (x)
  (/ (+ (oneway_mtld x) (oneway_mtld (reverse x))) 2))


(defun oneway_mtld(x)
  (let
      ((u 0.72)  ;;Untergrenze fuer TTR
       (f 0)     ;;Faktoren -> Anzahl der Textsegmente
       (c_ttr 1) ;;Derzeitiger TTR
       (sb 0)  ;;Segmentbeginn
       (l 1))   ;;Laufvariable   
    (loop
     for item in x
      do 
      (if (< c_ttr u) 
          (progn
            (unless (< (length (subseq x sb l)) 10)  (incf f))
            (setf sb (+ 1 l) ) 
            (setf c_ttr 1)
            ) 
          (setf c_ttr (simple_TTR (subseq x sb l))) 
        )
      (incf l)
      )
       (when (> c_ttr u) (setf f (+ f (/ (- 1 c_ttr) (- 1 u)))))
      (/ (list-length x) f)
    )
  )
      
;;;Formel f�r K�hler-Galle
(defun kgm_formel(PositionText AnzahlTypesPosition TypesText n)
  (float(/ (+ AnzahlTypesPosition (- TypesText (/ (* TypesText PositionText) n))) n))
   )

;;;Koehler-galle-Methode
;;;erh�lt Textobjekt und gibt Liste mit KG-Wert f�r jedes Token zur�ck
(defun kgm(x)
  (let* ((tokenized (Text-tokenized x))
         (gtz (typezahl tokenized))
         (n (list-length tokenized))
         (count 1)
         l)
    
  (loop
     for item in tokenized
    do
    (setf l (append l (list (kgm_formel count (typezahl (subseq tokenized 0 count)) gtz n))))
    (incf count))
    l)
  )
