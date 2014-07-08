;;; Code for the form named :form1 of class dialog.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)
;;;Set-up R for Porter
(defun set_r(x)
  (let* ((rend (length x)) (rstart rend) (vowel "a,i,u,e,o,y"))
  (loop
    for item across x
    do
    (if (find item vowel)
        (cond
         ((= (position item x) (- (length x) 1)) 
          (return))
         ((not (find (schar x (+ 1 (position item x))) vowel))
          (setq rstart (+ 2 (position item x)))(return)))))
    (if (>= rstart rend)
        ""
    (subseq x rstart rend))
    )
  )

;;;Test if is vowel
(defun test_vowel (character)
  (let ((vowel "a,i,u,e,o,y"))
  (if (find character vowel) character))
  )

;;;Test for Short-Syllable
;;;nimmt ein Wort und gibt die Position des Vokals der ersten gefundenen Silbe zurück
(defun test_short_syllable(word)
  
  (let ((pos 0))
  (loop
    for item across word
    do
       (unless (= pos (- (length word) 1)) 
         (if (= pos 0)
          (if (and (test_vowel item) (not(test_vowel (schar word 1))))
              (return pos))
          (if (and (test_vowel item)
                 (not (test_vowel(schar word (- pos 1)))) 
                 (not 
                       (or (test_vowel (schar word (+ pos 1))) 
                           (eql #\w (schar word (+ pos 1)))
                           (eql #\x (schar word (+ pos 1)))
                           (eql #\Y (schar word (+ pos 1))))))
            (return pos))))
    (incf pos)))            
  )

;;;Test for short
(defun test_short(word)
  (if (and (equal "" (set_r word)) (test_short_syllable (subseq word (- (length word) 3))))
       (if (< 0 (test_short_syllable (subseq word (- (length word) 3)))) "is")
  )
  )

;;;Test for double
(defun test_double(word)
  (cond
   ((search "bb" word) "bb")
   ((search "dd" word) "dd")
   ((search "ff" word) "ff")
   ((search "gg" word) "gg")
   ((search "mm" word) "mm")
   ((search "nn" word) "nn")
   ((search "pp" word) "pp")
   ((search "rr" word) "rr")
   ((search "tt" word) "tt")
 ))
  

  
;;;Test if contains vowel
;;;returns number of vowels, else NIL
(defun test_contains_vowel(word)
  (let ((count 0) (vowel "a,i,u,e,o,y"))
    (loop
      for item across word
      do
      (if (find item vowel) (setf count (+ 1 count)))) (unless (= count 0) count))
  )

;;;Porter-Stemmer
;;;item ist das zu stemmatisierende Wort
;;;Gibt das stemmatisierte Wort zurück
(defun porter_eng(item) 
  (when (> (length item) 2)
  ;;;Setzen von R1,R2 und dem li-ende
  (let*    ((r1 (set_r item)) 
           (r2 (set_r r1))
           (li_end "c,d,e,g,h,k,m,n,r,t"))
        
        ;;;Step 0:
        ;;;Suffixe ','s und 's'
        (cond 
         ((search "\'s\'" (subseq item (- (length item) 3)))
          (setf item (subseq item 0 (- (length item) 3))))
         ((search "\'s" (subseq item (- (length item) 2)))
          (setf item (subseq item 0 (- (length item) 2))))
         ((search "\'" (subseq item (- (length item) 1)))
              (setf item (subseq item 0 (- (length item) 1)))))
         
    ;;;Step 1a:
    ;;;Suffixe sses,ied,ies,s,ss,us
    (cond 
         ((search "sses" (subseq item (- (length item) 4)))
          (setf item (subseq item 0 (- (length item) 2))))
         ((search "ies" (subseq item (- (length item) 3)))
          (if (> (position #\i item :from-end t) 1)
              (setf item (subseq item 0 (- (length item) 2)))
              (setf item (subseq item 0 (- (length item) 1)))))
         ((search "ied" (subseq item (- (length item) 3)))
          (if (> (position #\i item :from-end t) 1)
              (setf item (subseq item 0 (- (length item) 2)))
            (setf item (subseq item 0 (- (length item) 1)))))
         ((search "ss" (subseq item (- (length item) 2)))
          )
         ((search "us" (subseq item (- (length item) 2)))
          )
         ((search "s" (subseq item (- (length item) 1)))
          (if (test_contains_vowel (subseq item 0 (- (length item) 2))) 
            (setf item (subseq item 0 (- (length item) 1))))))
 
    ;;;Step 1b:
    ;;;Suffixe eed,eedly,ed,edly,ing,ingly
    (cond
     ((and (search "eedly" (subseq item (- (length item) 5)))
           (search "eedly" r1))
      (setf item (subseq item 0 (- (length item) 3))))
     ((and (search "eed" (subseq item (- (length item) 3)))
           (search "eed" r1))
      (setf item (subseq item 0 (- (length item) 1))))
     ((and (search "ingly" (subseq item (- (length item) 5)))
           (test_contains_vowel (subseq item 0 (- (length item) 5))))
      (setf item (subseq item 0 (- (length item) 5)))
      (cond ((or (equal (subseq item (- (length item) 2)) "at")
                (equal (subseq item (- (length item) 2)) "bl")
                (equal (subseq item (- (length item) 2)) "iz"))
                (setf item (concatenate 'string item "e")))
            ((test_double (subseq item (- (length item) 2)))
                (setf item (subseq item 0 (- (length item) 1))))
            ((test_short item)
                (setf item (concatenate 'string item "e")))))
     ((and (search "ing" (subseq item (- (length item) 3)))
           (test_contains_vowel (subseq item 0 (- (length item) 3))))
      (setf item (subseq item 0 (- (length item) 3)))
      (cond ((or (equal (subseq item (- (length item) 2)) "at")
                (equal (subseq item (- (length item) 2)) "bl")
                (equal (subseq item (- (length item) 2)) "iz"))
                (setf item (concatenate 'string item "e")))
            ((test_double (subseq item (- (length item) 2)))
                (setf item (subseq item 0 (- (length item) 1))))
            ((test_short item)
                (setf item (concatenate 'string item "e")))))
     ((and (search "edly" (subseq item (- (length item) 4)))
           (test_contains_vowel (subseq item 0 (- (length item) 4))))
      (setf item (subseq item 0 (- (length item) 4)))
      (cond ((or (equal (subseq item (- (length item) 2)) "at")
                (equal (subseq item (- (length item) 2)) "bl")
                (equal (subseq item (- (length item) 2)) "iz"))
                (setf item (concatenate 'string item "e")))
            ((test_double (subseq item (- (length item) 2)))
                (setf item (subseq item 0 (- (length item) 1))))
            ((test_short item)
                (setf item (concatenate 'string item "e")))))
     ((and (search "ed" (subseq item (- (length item) 2)))
           (test_contains_vowel (subseq item 0 (- (length item) 2))))
      (setf item (subseq item 0 (- (length item) 2)))
      (cond ((or (equal (subseq item (- (length item) 2)) "at")
                (equal (subseq item (- (length item) 2)) "bl")
                (equal (subseq item (- (length item) 2)) "iz"))
                (setf item (concatenate 'string item "e")))
            ((test_double (subseq item (- (length item) 2)))
                (setf item (subseq item 0 (- (length item) 1))))
            ((test_short item)
                (setf item (concatenate 'string item "e")))))
     )

    ;;;Step 1c
    ;;;y and Y to i
    (cond
     ((search "y" (subseq item (- (length item) 1)))
      (if (< (test_contains_vowel (subseq item 1 (- (length item) 1))) (length (subseq item 1 (- (length item) 1))))
          (setf item (substitute #\i #\y item :count 1 :from-end t))))
     ((search "Y" (subseq item (- (length item) 1)))
      (if (< (test_contains_vowel (subseq item 1 (- (length item) 1))) (length (subseq item 1 (- (length item) 1))))
          (setf item (substitute #\i #\Y item :count 1 :from-end t))))
     )
    
    ;;;Step 1.5
    ;;;new R1
    (setf r1 (set_r item))
    
    ;;;Step 2
    ;;;a lot of suffixes
    
    (cond
     ((and (search "ational" (subseq item (- (length item) 7)))
           (search "ational" r1))
      (setf item (concatenate 'string (subseq item 0 (- (length item) 5)) "e")))
     ((and (search "ization" (subseq item (- (length item) 7)))
           (search "ization" r1))
      (setf item (concatenate 'string (subseq item 0 (- (length item) 5)) "e")))
     ((and (search "ation" (subseq item (- (length item) 5)))
           (search "ation" r1))
      (setf item (concatenate 'string (subseq item 0 (- (length item) 3)) "e")))
     ((and (search "ator" (subseq item (- (length item) 4)))
           (search "ator" r1))
      (setf item (concatenate 'string (subseq item 0 (- (length item) 2)) "e")))
     ((and (search "tional" (subseq item (- (length item) 6)))
           (search "tional" r1))
      (setf item (subseq item 0 (- (length item) 2))))
     ((and (search "izer" (subseq item (- (length item) 4)))
           (search "izer" r1))
      (setf item (subseq item 0 (- (length item) 1))))
     ((and (search "iveness" (subseq item (- (length item) 7)))
           (search "iveness" r1))
      (setf item (subseq item 0 (- (length item) 4))))
     ((and (search "iviti" (subseq item (- (length item) 5)))
           (search "iviti" r1))
      (setf item (concatenate 'string (subseq item 0 (- (length item) 3)) "e")))
     ((and (search "biliti" (subseq item (- (length item) 6)))
           (search "biliti" r1))
      (setf item (concatenate 'string (subseq item 0 (- (length item) 5)) "le")))
     ((and (search "bli" (subseq item (- (length item) 3)))
           (search "bli" r1))
      (setf item (concatenate 'string (subseq item 0 (- (length item) 1)) "e")))
     ((and (search "ousness" (subseq item (- (length item) 7)))
           (search "ousness" r1))
      (setf item (subseq item 0 (- (length item) 4))))
     ((and (search "ousli" (subseq item (- (length item) 5)))
           (search "ousli" r1))
      (setf item (subseq item 0 (- (length item) 2))))
     ((and (search "fulness" (subseq item (- (length item) 7)))
           (search "fulness" r1))
      (setf item (subseq item 0 (- (length item) 4))))
     ((and (search "lessli" (subseq item (- (length item) 6)))
           (search "lessli" r1))
      (setf item (subseq item 0 (- (length item) 2))))
     ((and (search "fulli" (subseq item (- (length item) 5)))
           (search "fulli" r1))
      (setf item (subseq item 0 (- (length item) 2))))
     ((and (search "iveness" (subseq item (- (length item) 7)))
           (search "iveness" r1))
      (setf item (subseq item 0 (- (length item) 4)))) 
     ((and (search "alism" (subseq item (- (length item) 5)))
           (search "alism" r1))
      (setf item (subseq item 0 (- (length item) 3))))
     ((and (search "aliti" (subseq item (- (length item) 5)))
           (search "aliti" r1))
      (setf item (subseq item 0 (- (length item) 3))))
     ((and (search "alli" (subseq item (- (length item) 4)))
           (search "alli" r1))
      (setf item (subseq item 0 (- (length item) 2))))
     ((and (search "entli" (subseq item (- (length item) 5)))
           (search "entli" r1))
      (setf item (subseq item 0 (- (length item) 2))))
     ((and (search "enci" (subseq item (- (length item) 4)))
           (search "enci" r1))
      (setf item (concatenate 'string (subseq item 0 (- (length item) 1)) "e")))
     ((and (search "anci" (subseq item (- (length item) 4)))
           (search "anci" r1))
      (setf item (concatenate 'string (subseq item 0 (- (length item) 1)) "e")))
     ((and (search "abli" (subseq item (- (length item) 4)))
           (search "abli" r1))
      (setf item (concatenate 'string (subseq item 0 (- (length item) 1)) "e")))
     ((and (search "ogi" (subseq item (- (length item) 3)))
           (search "ogi" r1)
           (search "l" (subseq item 0 (- (length item) 3))))
      (setf item (subseq item 0 (- (length item) 1))))
     ((and (search "li" (subseq item (- (length item) 2)))
           (search "li" r1)
           (search (subseq item (- (length item) 3) (- (length item) 2)) li_end))
      (setf item (subseq item 0 (- (length item) 2))))
     )
    
    ;;;Step 2.5
    ;;;new R1 R2
    (setf r1 (set_r item) r2 (set_r r1))
    
    
    ;;;Step 3
    ;;;a few more suffixes
    (cond
      ((and (search "ational" (subseq item (- (length item) 7)))
           (search "ational" r1))
       (setf item (concatenate 'string (subseq item 0 (- (length item) 5)) "e")))
      ((and (search "tional" (subseq item (- (length item) 6)))
           (search "tional" r1))
       (setf item (subseq item 0 (- (length item) 2))))
      ((and (search "alize" (subseq item (- (length item) 5)))
           (search "alize" r1))
      (setf item (subseq item 0 (- (length item) 3))))
      ((and (search "icate" (subseq item (- (length item) 5)))
           (search "icate" r1))
       (setf item (subseq item 0 (- (length item) 3))))
      ((and (search "iciti" (subseq item (- (length item) 5)))
           (search "iciti" r1))
       (setf item (subseq item 0 (- (length item) 3))))
      ((and (search "ical" (subseq item (- (length item) 4)))
           (search "ical" r1))
       (setf item (subseq item 0 (- (length item) 2))))
      ((and (search "ness" (subseq item (- (length item) 4)))
           (search "ness" r1))
       (setf item (subseq item 0 (- (length item) 4))))
      ((and (search "ful" (subseq item (- (length item) 3)))
           (search "ful" r1))
       (setf item (subseq item 0 (- (length item) 3))))
      ((and (search "ative" (subseq item (- (length item) 5)))
            (search "ative" r1)
            (search "ative" r2))
       (setf item (subseq item 0 (- (length item) 5))))
     )
    
    ;;;Step 3.5
    ;;;new R1 R2
    (setf r1 (set_r item) r2 (set_r r1))
    
    ;;;Step 4
    ;;;some more suffixes in r2
    (cond
     ((and (search "al" (subseq item (- (length item) 2)))
           (search "al" r2))
      (setf item (subseq item 0 (- (length item) 2))))
     ((and (search "ance" (subseq item (- (length item) 4)))
           (search "ance" r2))
      (setf item (subseq item 0 (- (length item) 4))))
     ((and (search "ence" (subseq item (- (length item) 4)))
           (search "ence" r2))
      (setf item (subseq item 0 (- (length item) 4))))
     ((and (search "er" (subseq item (- (length item) 2)))
           (search "er" r2))
      (setf item (subseq item 0 (- (length item) 2))))
     ((and (search "ic" (subseq item (- (length item) 2)))
           (search "ic" r2))
      (setf item (subseq item 0 (- (length item) 2))))
     ((and (search "able" (subseq item (- (length item) 4)))
           (search "able" r2))
      (setf item (subseq item 0 (- (length item) 4))))
     ((and (search "ible" (subseq item (- (length item) 4)))
           (search "ible" r2))
      (setf item (subseq item 0 (- (length item) 4))))
     ((and (search "ant" (subseq item (- (length item) 3)))
           (search "ant" r2))
      (setf item (subseq item 0 (- (length item) 3))))
     ((and (search "ement" (subseq item (- (length item) 5)))
           (search "ement" r2))
      (setf item (subseq item 0 (- (length item) 5))))
     ((and (search "ment" (subseq item (- (length item) 4)))
           (search "ment" r2))
      (setf item (subseq item 0 (- (length item) 4))))
     ((and (search "ent" (subseq item (- (length item) 3)))
           (search "ent" r2))
      (setf item (subseq item 0 (- (length item) 3))))
     ((and (search "ism" (subseq item (- (length item) 3)))
           (search "ism" r2))
      (setf item (subseq item 0 (- (length item) 3))))
     ((and (search "ate" (subseq item (- (length item) 3)))
           (search "ate" r2))
      (setf item (subseq item 0 (- (length item) 3))))
     ((and (search "iti" (subseq item (- (length item) 3)))
           (search "iti" r2))
      (setf item (subseq item 0 (- (length item) 3))))
     ((and (search "ous" (subseq item (- (length item) 3)))
           (search "ous" r2))
      (setf item (subseq item 0 (- (length item) 3))))
     ((and (search "ive" (subseq item (- (length item) 3)))
           (search "ive" r2))
      (setf item (subseq item 0 (- (length item) 3))))
     ((and (search "ize" (subseq item (- (length item) 3)))
           (search "ize" r2))
      (setf item (subseq item 0 (- (length item) 3))))
     ((and (search "ion" (subseq item (- (length item) 3)))
           (search "ion" r2)
           (or (find #\t (subseq item 0 (- (length item) 3)))
               (find #\s (subseq item 0 (- (length item) 3)))))
       (setf item (subseq item 0 (- (length item) 3))))
     )
    ;;;Step 5
    ;;; l und e
    (cond
     ((and (search "l" (subseq item (- (length item) 1)))
           (search "l" r2)
           (search "l" (subseq item 0 (- (length item) 1))))
      (setf item (subseq item 0 (- (length item) 1))))
     ((and (search "e" (subseq item (- (length item) 1)))
           (or (search "e" r2)
               (and (search "e" r1)
                    (test_short_syllable r1)
                    (not (< (test_short_syllable r1) (search "e" r1))))))
      (setf item (subseq item 0 (-(length item) 1)))))
     
	))
       ;;;Ausgabe des stemmatisierten Wortes
       item)




(defun naive_tokenizer(x)
    (let
    ((s "") l)  
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
      (unless (equal s "") (prog1 (setq l (append l (list (string-downcase s)))) (setq s "")))
      l)
  )

;;;Erhält eine eine Liste mit Satztokens
;;;Ruft für jedes Element dieser Liste "Porter_german" aus
;;;Gibt die stemmatisierte Tokenliste zurück
;;;Arbeitet rekursiv!
(defun iterate_porter(x)
  (when x
    (append (list (porter_eng (car x))) (iterate_porter (cdr x)))))


(defun form1-button5-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  (setf
   (value (find-sibling :multi-line-editable-text-2 widget))
   (iterate_porter (naive_tokenizer (value (find-sibling :multi-line-editable-text-1 widget))))
   (value (find-sibling :multi-line-editable-text-3 widget))
   (set_r (value (find-sibling :multi-line-editable-text-1 widget)))
   (value (find-sibling :multi-line-editable-text-4 widget))
   (set_r "knite")
   (value (find-sibling :multi-line-editable-text-5 widget))
   (test_short_syllable "e")
   )
  t)
