(in-package :common-graphics-user)

;;;Implementation of Porter Stemmer for the german language
;;;FOllowing this description:
;;; http://snowball.tartarus.org/algorithms/german/stemmer.html

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


;;;Porter-Stemmer
;;;item ist das zu stemmatisierende Wort
;;;Gibt das stemmatisierte Wort zurück
(defun porter_german(item)
  
  ;;;Ersetzung der Umlaute und von ß
  (setf item (substitute #\a #\ä item)
        item (substitute #\o #\ö item)
        item (substitute #\u #\ü item)
    )
  (when (find #\ß item) 
    (setf item (concatenate 'string (subseq item 0 (position #\ß item)) "ss" (subseq item (+ (position #\ß item) 1)))))
  
  ;;;Setzen von R1,R2, dem validen S_Ende sowie St.Ende
      (let* 
          ((r1 (set_r item)) 
           (r2 (set_r r1))
           (s_end "b,d,f,g,h,k,l,m,n,r,t")
           (st_end "b,d,f,g,h,k,l,m,n,t"))
        
        ;;;Schritt 1:
        ;;;Suffixe em,ern,er,en,es,e,s
        (cond 
         ((and (search "ern" (subseq item (- (length item) 3))) (search "ern" r1))
              (setf item (subseq item 0 (- (length item) 3))))
         ((and (search "em" (subseq item (- (length item) 2))) (search "em" r1))
              (setf item (subseq item 0 (- (length item) 2))))
         ((and (search "er" (subseq item (- (length item) 2))) (search "er" r1))
              (setf item (subseq item 0 (- (length item) 2))))
         ((and (search "en" (subseq item (- (length item) 2))) (search "en" r1))
          (if
              (equal "niss" (subseq item (- (length item) 6) (- (length item) 2)))
              (setf item (subseq item 0 (- (length item) 3)))
	      (setf item (subseq item 0 (- (length item) 2)))
          ))
         ((and (search "es" (subseq item (- (length item) 2))) (search "es" r1))
          (if
              (equal "niss" (subseq item (- (length item) 6) (- (length item) 2)))
              (setf item (subseq item 0 (- (length item) 3)))
	      (setf item (subseq item 0 (- (length item) 2)))
          ))
         ((and (search "e" (subseq item (- (length item) 1))) (search "e" r1))
          (if
              (equal "niss" (subseq item (- (length item) 5) (- (length item) 1)))
              (setf item (subseq item 0 (- (length item) 2)))
	      (setf item (subseq item 0 (- (length item) 1)))
          ))
         ((and (search "s" (subseq item (- (length item) 1)))
	       (search "s" r1) 
	       (search (subseq item (- (length item) 2) (- (length item) 1)) s_end))
           (setf item (subseq item 0 (- (length item) 1))))
         )
        
        ;;;Schritt 2:
        ;;;Suffixe est,er,en,st
        (cond 
         ((and (search "est" (subseq item (- (length item) 3))) (search "est" r1))
              (setf item (subseq item 0 (- (length item) 3))))
         ((and (search "en" (subseq item (- (length item) 2))) (search "en" r1))
              (setf item (subseq item 0 (- (length item) 2))))
         ((and (search "er" (subseq item (- (length item) 2))) (search "er" r1))
              (setf item (subseq item 0 (- (length item) 2))))
         ((and (search "st" (subseq item (- (length item) 2)))
               (search "st" r1))
          (if (and (search (subseq item (- (length item) 3) (- (length item) 2)) st_end)
                   (>= (search "st" item) 4)) 
                    (setf item (subseq item 0 (- (length item) 2)))))
         )

        
        ;;;Schritt 3:
        ;;;Suffixe lich,heit,keit,end,ung,isch,ik,ig
        (cond
         ((and (search "lich" (subseq item (- (length item) 4))) (search "lich" r2))
	  (setf item (subseq item 0 (- (length item) 4)))
          (if (and (search "er" item) (search "er" r1))
	      (setf item (subseq item 0 (search "er" item))))
	  (if (and (search "en" item) (search "en" r1))
	      (setf item (subseq item 0 (search "en" item)))))
         ((and (search "heit" (subseq item (- (length item) 4))) (search "heit" r2))
	  (setf item (subseq item 0 (- (length item) 4)))
          (if (and (search "er" item) (search "er" r1))
	      (setf item (subseq item 0 (search "er" item))))
	  (if (and (search "en" item) (search "en" r1))
	      (setf item (subseq item 0 (search "en" item)))))
	 ((and (search "keit" (subseq item (- (length item) 4))) (search "keit" r2))
	  (setf item (subseq item 0 (- (length item) 4)))
          (if (and (search "lich" item) (search "lich" r2))
	      (setf item (subseq item 0 (search "lich" item))))
	  (if (and (search "ig" item) (search "ig" r2))
	      (setf item (subseq item 0 (search "lich" item)))))	
         ((and (search "end" (subseq item (- (length item) 3))) (search "end" r2))
	  (setf item (subseq item 0 (- (length item) 3)))
	  (if (and (search "ig" item) (search "ig" r2))
	      (unless (equal "e" (subseq r2 0 (search "ig" r2)))
		      (setf item (subseq item 0 (search "ig" item))))))
         ((and (search "ung" (subseq item (- (length item) 3))) (search "ung" r2))
	  (setf item (subseq item 0 (- (length item) 3)))
	  (if (and (search "ig" item) (search "ig" r2))
	      (unless (equal "e" (subseq r2 0 (search "ig" r2)))
		      (setf item (subseq item 0 (search "ig" item))))))
         ((and (search "isch" (subseq item (- (length item) 4))) (search "isch" r2))
          (unless (search "e" (subseq r2 0 (search "isch" r2)))
		  (setf item (subseq item 0 (- (length item) 4)))))
	 ((and (search "ik" (subseq item (- (length item) 2))) (search "ik" r2))
          (unless (search "e" (subseq r2 0 (search "ik" r2)))
		  (setf item (subseq item 0 (- (length item) 2)))))
	  ((and (search "ig" (subseq item (- (length item) 2))) (search "ig" r2))
           (unless (search "e" (subseq r2 0 (search "ig" r2)))
		  (setf item (subseq item 0 (- (length item) 2)))))
	)
       ;;;Ausgabe des stemmatisierten Wortes
        item))


;;;Erhält eine eine Liste mit Satztokens
;;;Ruft für jedes Element dieser Liste "Porter_german" aus
;;;Gibt die stemmatisierte Tokenliste zurück
;;;Arbeitet rekursiv!
(defun iterate_german(x)
  (when x
    (append (list (porter_german(car x))) (iterate_german (cdr x)))))


