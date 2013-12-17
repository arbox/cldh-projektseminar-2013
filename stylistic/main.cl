;;; Code for the form named :main of class dialog.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)

;;;Naiver Tokenizer ohne Berücksichtigung von Lemmata.
;;;Trennt Wörter anhand von Leerzeichen und anderen Sonderzeichen
;;;Nimmt einen String x und gibt eine Liste mit allen Tokens aus
(defun naive_tokenizer(x)
    (let
    ((s "") (l '()))  
    (loop
    for item across x
      do (cond
          ((or (char= item #\Space) (char= item #\.) 
               (char= item #\!) (char= item #\?) 
               (char= item #\:) (char= item #\,) 
               (char= item #\;) (char= item #\"))
           (unless (equal s "") (prog1 (setq l (append l (list (string-downcase s)))) (setq s ""))))
          (t
           (setq s (concatenate 'string s (string item)))))
      )
      (setq l (append l (list (string-downcase s))))
      l)
    )

;;;Erhält eine Liste x mit Tokens und zählt alle darin vorkommende Types
(defun typezahl(x)
  (let
      ((c 0) (type 0))
  (loop
   for item in x
    do 
    (setq type 0)
    (loop
         for i in x until (= (position i x) (position item x))
         do
         (if (equal i item)
             (setq type 1))
    )
    (if (= type 0) (setq c (+ c 1)))
    )
  c)
  ) 

;;;Erhält eine Liste x Tokens und errechnet die TTR indem es die Anzahl der Types durch Anzahl der Tokens dividiert
(defun simple_TTR(x)
  (if (= (length x) 0)
      1
      (/ (typezahl x) (length x))))

;;;Erhält eine Liste x mit Tokens und errechent den Measure of Textual Diversity nach McCarthy (2005)
;;;muss noch getestes werden !!!
(defun mtld(x)
  (let
      ((u 0.71)  ;;Untergrenze fuer TTR
       (f 1)     ;;Faktoren / Anzahl der Textsegmente
       (c_ttr 1) ;;Derzeitiger TTR
       (sb (first x))  ;;Segmentbeginn
       (rl (reverse x))  ;;Tokenliste von hinten    
       (e))   ;;Ergebnis
    
      
    (loop
     for item in x
      do 
      (if (< c_ttr u) 
          (progn
            (setf sb item) 
            (setf f (+ f 1))
            (setf c_ttr 1)) 
          (setf c_ttr (simple_TTR (subseq x (position sb x) (position item x)))) 
               )
      )
       (setf e (/ (list-length x) f))
       (setf c_ttr 1)
       (setf f 1)
       (setf sb (first rl))
    (loop
     for item in rl
      do 
      (if (< c_ttr u) 
          (progn
            (setf sb item) 
            (setf f (+ f 1))
            (setf c_ttr 1)) 
          (setf c_ttr (simple_TTR (subseq rl (position sb rl) (position item rl)))) 
               )
      )
    (setf e (+ e (/ (list-length rl) f)))
    (setf e (/ e 2))  
      ) 
    )
       
;;;Generiert die Ausgabe                  
(defun output (x)
  (concatenate 'string "Die Type-Token-Ratio ist: "(write-to-string (float (simple_TTR (naive_tokenizer x))))"
Die MTLD ist:" (write-to-string(mtld(naive_tokenizer x))) 
    ) 
  )

;;;;;;;;;;;;;;;;;;
;;;Buttons
;;;etc
;;;;;;;;;;;;;;;;;;

;;;Ereignis des Analyse1-Buttons
(defun main-analyse1-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  (setf (value (find-sibling :output1 widget)) (output (value (find-sibling :input1 widget))))
  t)

;;;Ereignis des Analyse2-Buttons
(defun main-analyse2-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  (setf (value (find-sibling :output2 widget)) (output (value (find-sibling :input2 widget))))
  t)

;;;Ereignis des Menüpunkts "Open"
(defun datei-laden(main)
  (let
      ((c (ask-user-for-choice-from-list "In welches Fenster soll der Text geladne werden?" (list 1 2) )))
  (case c
    (1   (setf (value(find-named-object :input1 main)) (file-contents(ask-user-for-existing-pathname "Dateien Laden"))))
    (2   (setf (value(find-named-object :input2 main)) (file-contents(ask-user-for-existing-pathname "Dateien Laden")))
     )
    )
    )
  )