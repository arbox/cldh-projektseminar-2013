
(in-package :common-graphics-user)
;;;Buchstabe 1104???
(defun trans_rus(word)
  (let ((result ""))
  (loop
    for item across word
    do
    (case (char-code item)
     (1072
      (setf result (concatenate 'string result "a")))
     (1073
      (setf result (concatenate 'string result "b")))
     (1074
      (setf result (concatenate 'string result "v")))
     (1075
      (setf result (concatenate 'string result "g")))
     (1076
      (setf result (concatenate 'string result "d")))
     (1077
      (setf result (concatenate 'string result "e")))
     (1078
      (setf result (concatenate 'string result "Zh")))
     (1079
      (setf result (concatenate 'string result "z")))
     (1080
      (setf result (concatenate 'string result "i")))
     (1081
      (setf result (concatenate 'string result "ì")))
     (1082
      (setf result (concatenate 'string result "k")))
     (1083
      (setf result (concatenate 'string result "l")))
     (1084
      (setf result (concatenate 'string result "m")))
     (1085
      (setf result (concatenate 'string result "n")))
     (1086
      (setf result (concatenate 'string result "o")))
     (1087
      (setf result (concatenate 'string result "p")))
     (1088
      (setf result (concatenate 'string result "r")))
     (1089
      (setf result (concatenate 'string result "s")))
     (1090
      (setf result (concatenate 'string result "t")))
     (1091
      (setf result (concatenate 'string result "u")))
     (1092
      (setf result (concatenate 'string result "f")))
     (1093
      (setf result (concatenate 'string result "Kh")))
     (1094
      (setf result (concatenate 'string result "Ts")))
     (1095
      (setf result (concatenate 'string result "Ch")))
     (1096
      (setf result (concatenate 'string result "Sh")))
     (1097
      (setf result (concatenate 'string result "Shch")))
     (1098
      (setf result (concatenate 'string result "\"")))
     (1099
      (setf result (concatenate 'string result "y")))
     (1100
      (setf result (concatenate 'string result "\'")))
     (1101
      (setf result (concatenate 'string result "è")))
     (1102
      (setf result (concatenate 'string result "Iu")))
     (1103
      (setf result (concatenate 'string result "Ia")))
     )
    )
    result 
  )
  )

;;;Russ -> English
(defun retrans(word)
  (let
      ((result '()))
      (loop for i upto (- (length word) 1)
          do
        (case (char word i)
          (#\a
           (setf result (append result (list(code-char 1072)))))
          (#\b
           (setf result (append result (list(code-char 1073)))))
          (#\v
           (setf result (append result (list(code-char 1074)))))
          (#\g
           (setf result (append result (list(code-char 1075)))))
          (#\d
           (setf result (append result (list(code-char 1076)))))
          (#\e
           (setf result (append result (list(code-char 1077)))))
          (#\Z
           (setf result (append result (list(code-char 1078))))
           (incf i))
          (#\z
           (setf result (append result (list(code-char 1079)))))
          (#\i
           (setf result (append result (list(code-char 1080)))))
          (#\ì
           (setf result (append result (list(code-char 1081)))))
          (#\k
           (setf result (append result (list(code-char 1082)))))
          (#\l
           (setf result (append result (list(code-char 1083)))))
          (#\m
           (setf result (append result (list(code-char 1084)))))
          (#\n
           (setf result (append result (list(code-char 1085)))))
          (#\o
           (setf result (append result (list(code-char 1086)))))
          (#\p
           (setf result (append result (list(code-char 1087)))))
          (#\r
           (setf result (append result (list(code-char 1088)))))
          (#\s
           (setf result (append result (list(code-char 1089)))))
          (#\t
           (setf result (append result (list(code-char 1090)))))
          (#\u
           (setf result (append result (list(code-char 1091)))))
          (#\f
           (setf result (append result (list(code-char 1092)))))
          (#\K
           (setf result (append result (list(code-char 1093))))
           (incf i))
          (#\T
           (setf result (append result (list(code-char 1094))))
           (incf i))
          (#\C
           (setf result (append result (list(code-char 1095))))
           (incf i))
          (#\S
           (if (and 
                (< (+ i 2) (length word))
                (eq (char word (+ 2 i)) #\c))
               (progn
                 (setf result (append result (list(code-char 1097))))
                 (setf i (+ i 3)))
             (progn
                 (setf result (append result (list(code-char 1096))))
               (incf i))
             ))
          (#\"
           (setf result (append result (list(code-char 1098)))))
          (#\y
           (setf result (append result (list(code-char 1099)))))
          (#\'
           (setf result (append result (list(code-char 1100)))))
          (#\è
           (setf result (append result (list(code-char 1101)))))
          (#\I
           (if (and
                (< (+ i 1) (length word))
                (eq (char word (+ 1 i)) #\u))
               (progn
                 (setf result (append result (list(code-char 1102))))
                 (incf i))
             (progn
                 (setf result (append result (list(code-char 1103))))
               (incf i))
             )
          )
          )
            )
    (coerce result 'string)
    )
  )

;;;Test if is vowel
(defun test_vowel (character)
  (let ((vowel "a,i,u,e,o,y,è"))
  (if (find character vowel) character))
  )


;;;Findet den Beginn von RV für den russischen Porter
;;;Gibt die Starposition zurück
(defun start_rv(word)
  (let ((x (length word)))
  (loop
    for item across word
    do
    (if (test_vowel item)
         (unless (= (position item word) (- (length word) 1))
          (progn (setf x (+ (position item word) 1)) (return)))
      ))
    x))

;;;Set-up R from german Porter

;;;TestGerund
(defun gerund(word)
  (let ((length (length word)) (cut 0))
    (cond
     ((search "yvShis\'" (subseq word (- length 7)))
      (setf cut 7))
     ((search "ivShis\'" (subseq word (- length 7)))
      (setf cut 7))
     ((search "yvShi" (subseq word (- length 5)))
      (setf cut 5))
     ((search "ivShi" (subseq word (- length 5)))
      (setf cut 5))
     ((search "yv" (subseq word (- length 2)))
      (setf cut 2))
     ((search "iv" (subseq word (- length 2)))
      (setf cut 2))
     ((search "avShis\'" (subseq word (- length 7)))
      (setf cut 6))
     ((search "avShi" (subseq word (- length 5)))
      (setf cut 4))
     ((search "av" (subseq word (- length 2)))
      (setf cut 1))
     )
    cut))

;;;TestAdjective
(defun adjective(word)
  (let ((length (length word)) (cut 0))
    (cond
     ((search "ee" (subseq word (- length 2)))
      (setf cut 2))
     ((search "ie" (subseq word (- length 2)))
      (setf cut 2))
     ((search "ye" (subseq word (- length 2)))
      (setf cut 2))
     ((search "oe" (subseq word (- length 2)))
      (setf cut 2))
     ((search "eì" (subseq word (- length 2)))
      (setf cut 2))
     ((search "iì" (subseq word (- length 2)))
      (setf cut 2))
     ((search "yì" (subseq word (- length 2)))
      (setf cut 2))
     ((search "oì" (subseq word (- length 2)))
      (setf cut 2))
     ((search "em" (subseq word (- length 2)))
      (setf cut 2))
     ((search "im" (subseq word (- length 2)))
      (setf cut 2))
     ((search "ym" (subseq word (- length 2)))
      (setf cut 2))
     ((search "om" (subseq word (- length 2)))
      (setf cut 2))
     ((search "imi" (subseq word (- length 3)))
      (setf cut 3))
     ((search "ymi" (subseq word (- length 3)))
      (setf cut 3))
     ((search "ego" (subseq word (- length 3)))
      (setf cut 3))
     ((search "ogo" (subseq word (- length 3)))
      (setf cut 3))
     ((search "emu" (subseq word (- length 3)))
      (setf cut 3))
     ((search "omu" (subseq word (- length 3)))
      (setf cut 3))
     ((search "iKh" (subseq word (- length 3)))
      (setf cut 3))
     ((search "yKh" (subseq word (- length 3)))
      (setf cut 3))
     ((search "oIu" (subseq word (- length 3)))
      (setf cut 3))
     ((search "eIu" (subseq word (- length 3)))
      (setf cut 3))
     ((search "IuIu" (subseq word (- length 4)))
      (setf cut 4))
     ((search "IaIa" (subseq word (- length 4)))
      (setf cut 4))
     ((search "uIu" (subseq word (- length 3)))
      (setf cut 3))
     ((search "aIa" (subseq word (- length 3)))
      (setf cut 3))
     )
    cut))

;;;TestParticle
(defun particle(word)
  (let ((length (length word)) (cut 0))
    (cond
     ((search "uIuShch" (subseq word (- length 7)))
      (setf cut 7))
     ((search "yvSh" (subseq word (- length 4)))
      (setf cut 4))
     ((search "ivSh" (subseq word (- length 4)))
      (setf cut 4))
     ((search "aIuShch" (subseq word (- length 7)))
      (setf cut 6))
     ((search "aShch" (subseq word (- length 5)))
      (setf cut 4))
     ((search "avSh" (subseq word (- length 4)))
      (setf cut 3))
     ((search "ann" (subseq word (- length 3)))
      (setf cut 2))
     ((search "aem" (subseq word (- length 3)))
      (setf cut 2))
     )
    cut))

;;;Test Reflexive
(defun reflexive(word)
  (let ((cut 0))
    (cond
     ((search "sIa" (subseq word (- (length word) 3)))
      (setf cut 3))
     ((search "s\'" (subseq word (- (length word) 2)))
      (setf cut 2))
     )
    cut))

;;;Test Verb
(defun verb(word)
  (let ((length (length word)) (cut 0))
    (cond
     ((search "ila" (subseq word (- length 3)))
      (setf cut 3))
     ((search "yla" (subseq word (- length 3)))
      (setf cut 3))
     ((search "ena" (subseq word (- length 3)))
      (setf cut 3))
     ((search "eìte" (subseq word (- length 4)))
      (setf cut 4))
     ((search "uìte" (subseq word (- length 4)))
      (setf cut 4))
     ((search "ite" (subseq word (- length 3)))
      (setf cut 3))
     ((search "ili" (subseq word (- length 3)))
      (setf cut 3))
     ((search "yli" (subseq word (- length 3)))
      (setf cut 3))
     ((search "eì" (subseq word (- length 2)))
      (setf cut 2))
     ((search "uì" (subseq word (- length 2)))
      (setf cut 2))
     ((search "il" (subseq word (- length 2)))
      (setf cut 2))
     ((search "yl" (subseq word (- length 2)))
      (setf cut 2))
     ((search "im" (subseq word (- length 2)))
      (setf cut 2))
     ((search "ym" (subseq word (- length 2)))
      (setf cut 2))
     ((search "en" (subseq word (- length 2)))
      (setf cut 2))
     ((search "ilo" (subseq word (- length 3)))
      (setf cut 3))
     ((search "ylo" (subseq word (- length 3)))
      (setf cut 3))
     ((search "eno" (subseq word (- length 3)))
      (setf cut 3))
     ((search "Iat" (subseq word (- length 3)))
      (setf cut 3))
     ((search "uet" (subseq word (- length 3)))
      (setf cut 3))
     ((search "uIut" (subseq word (- length 4)))
      (setf cut 4))
     ((search "it" (subseq word (- length 2)))
      (setf cut 2))
     ((search "yt" (subseq word (- length 2)))
      (setf cut 2))
     ((search "eny" (subseq word (- length 3)))
      (setf cut 3))
     ((search "it\'" (subseq word (- length 3)))
      (setf cut 3))
     ((search "yt\'" (subseq word (- length 3)))
      (setf cut 3))
     ((search "iSh\'" (subseq word (- length 4)))
      (setf cut 4))
     ((search "uIu" (subseq word (- length 3)))
      (setf cut 3))
     ((search "Iu" (subseq word (- length 2)))
      (setf cut 2))
     ((search "ala" (subseq word (- length 3)))
      (setf cut 2))
     ((search "ana" (subseq word (- length 3)))
      (setf cut 2))
     ((search "aete" (subseq word (- length 4)))
      (setf cut 3))
     ((search "aìte" (subseq word (- length 4)))
      (setf cut 3))
     ((search "ali" (subseq word (- length 3)))
      (setf cut 2))
     ((search "aì" (subseq word (- length 2)))
      (setf cut 1))
     ((search "al" (subseq word (- length 2)))
      (setf cut 1))
     ((search "aem" (subseq word (- length 3)))
      (setf cut 2))
     ((search "an" (subseq word (- length 2)))
      (setf cut 1))
     ((search "alo" (subseq word (- length 3)))
      (setf cut 2))
     ((search "ano" (subseq word (- length 3)))
      (setf cut 2))
     ((search "aet" (subseq word (- length 3)))
      (setf cut 2))
     ((search "aIut" (subseq word (- length 4)))
      (setf cut 3))
     ((search "any" (subseq word (- length 3)))
      (setf cut 2))
     ((search "at\'" (subseq word (- length 3)))
      (setf cut 2))
     ((search "aeSh\'" (subseq word (- length 5)))
      (setf cut 4))
     ((search "anno" (subseq word (- length 4)))
      (setf cut 3))
     )
    cut))

;;;Test noun
(defun noun(word)
  (let ((length (length word)) (cut 0))
    (cond
     ((search "ev" (subseq word (- length 2)))
      (setf cut 2))
     ((search "ov" (subseq word (- length 2)))
      (setf cut 2))
     ((search "ie" (subseq word (- length 2)))
      (setf cut 2))
     ((search "\'e" (subseq word (- length 2)))
      (setf cut 2))
     ((search "e" (subseq word (- length 1)))
      (setf cut 1))
     ((search "iIami" (subseq word (- length 5)))
      (setf cut 5))
     ((search "Iami" (subseq word (- length 4)))
      (setf cut 4))
     ((search "ami" (subseq word (- length 3)))
      (setf cut 3))
     ((search "ieì" (subseq word (- length 3)))
      (setf cut 3))
     ((search "eì" (subseq word (- length 2)))
      (setf cut 2))
     ((search "oì" (subseq word (- length 2)))
      (setf cut 2))
     ((search "iì" (subseq word (- length 2)))
      (setf cut 2))
     ((search "ì" (subseq word (- length 1)))
      (setf cut 1))
     ((search "ei" (subseq word (- length 2)))
      (setf cut 2))
     ((search "ii" (subseq word (- length 2)))
      (setf cut 2))
     ((search "i" (subseq word (- length 1)))
      (setf cut 1))
     ((search "iIam" (subseq word (- length 4)))
      (setf cut 4))
     ((search "Iam" (subseq word (- length 3)))
      (setf cut 3))
     ((search "iem" (subseq word (- length 3)))
      (setf cut 3))
     ((search "em" (subseq word (- length 2)))
      (setf cut 2))
     ((search "am" (subseq word (- length 2)))
      (setf cut 2))
     ((search "om" (subseq word (- length 2)))
      (setf cut 2))
     ((search "o" (subseq word (- length 1)))
      (setf cut 1))
     ((search "u" (subseq word (- length 1)))
      (setf cut 1))
     ((search "aKh" (subseq word (- length 3)))
      (setf cut 3))
     ((search "iIaKh" (subseq word (- length 5)))
      (setf cut 5))
     ((search "IaKh" (subseq word (- length 4)))
      (setf cut 4))
     ((search "y" (subseq word (- length 1)))
      (setf cut 1))
     ((search "\'" (subseq word (- length 1)))
      (setf cut 1))
     ((search "iIu" (subseq word (- length 3)))
      (setf cut 3))
     ((search "\'Iu" (subseq word (- length 3)))
      (setf cut 3))
     ((search "Iu" (subseq word (- length 2)))
      (setf cut 2))
     ((search "iIa" (subseq word (- length 3)))
      (setf cut 3))
     ((search "\'Ia" (subseq word (- length 3)))
      (setf cut 3))
     ((search "Ia" (subseq word (- length 2)))
      (setf cut 2))
     ((search "a" (subseq word (- length 1)))
      (setf cut 1))
     )
    cut))

;;;Test superlative
(defun superlative(word)
  (let ((cut 0))
    (cond
     ((search "eìSh" (subseq word (- (length word) 4)))
      (setf cut 4))
     ((search "eìShe" (subseq word (- (length word) 5)))
      (setf cut 5))
     )
    cut))

;;;Test derivational
(defun derivational(word)
  (let ((cut 0))
    (cond
     ((search "ost\'" (subseq word (- (length word) 4)))
      (setf cut 4))
     ((search "ost" (subseq word (- (length word) 3)))
      (setf cut 3))
     )
    cut))

;;;Test adjectival
(defun adjectival(word)
  (+ (adjective word) (particle (subseq word 0 (- (length word) (adjective word)))))
  )

;;;Stemmer
(defun porter_rus(word)
  (let ((rv (subseq word (start_rv word) (length word)))
        (r2 (set_r (set_r word))))

    ;;;Step 1
    (cond
     ((not (= 0 (gerund rv)))
      (setf rv (subseq rv 0  (- (length rv) (gerund rv)))))
     ((not (= 0 (reflexive rv)))
      (setf rv (subseq rv 0 (- (length rv) (reflexive rv))))
      (cond
       ((not (= 0 (adjectival rv)))
        (setf rv (subseq rv 0 (- (length rv) (adjectival rv)))))
       ((not (= 0 (verb rv)))
        (setf rv (subseq rv 0 (- (length rv) (verb rv)))))
       ((not (= 0 (noun rv)))
        (setf rv (subseq rv 0 (- (length rv) (noun rv)))))
       ))
     ((or
       (not (= 0 (adjectival rv)))
       (not (= 0 (verb rv)))
       (not (= 0 (noun rv))))
      (cond
       ((not (= 0 (adjectival rv)))
        (setf rv (subseq rv 0 (- (length rv) (adjectival rv)))))
       ((not (= 0 (verb rv)))
        (setf rv (subseq rv 0 (- (length rv) (verb rv)))))
       ((not (= 0 (noun rv)))
        (setf rv (subseq rv 0 (- (length rv) (noun rv)))))
       )
      )
     )
    
    ;;;Step 2
    (if
        (search "i"(subseq rv (- (length rv) 1)))
        (setf rv (subseq rv 0 (- (length rv) 1)))
      )
    
    ;;;Step 3
    (if 
        (not (= 0 (derivational r2)))
        (setf rv (subseq rv 0 (- (length rv) (derivational rv))))
      )
    
    ;;;Step 4
    (cond
     ((search "nn"(subseq rv (- (length rv) 2)))
      (setf rv (subseq rv 0 (- (length rv) 1))))
     ((not (= 0 (superlative rv)))
      (setf rv (subseq rv 0 (- (length rv) (superlative rv))))
      (if
          (search "nn"(subseq rv (- (length rv) 2)))
          (setf rv (subseq rv 0 (- (length rv) 1))))
      )
     ((search "\'"(subseq rv (- (length rv) 1)))
     (setf rv (subseq rv 0 (- (length rv) 1))))
     )
    
    ;;;Rest von RV + Wortanfang
    (concatenate 'string (subseq word 0 (start_rv word)) rv)
    )
  )

;;;Erhält eine eine Liste mit Satztokens
;;;Ruft für jedes Element dieser Liste "Porter_german" aus
;;;Gibt die stemmatisierte Tokenliste zurück
;;;Arbeitet rekursiv!
(defun iterate_russian(x)
  (when x
    (append (list (retrans(porter_rus(trans_rus (car x))))) (iterate_russian (cdr x)))))

(defun form1-button5-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  (setf
   (value (find-sibling :multi-line-editable-text-2 widget))
   (retrans(porter_rus(trans_rus (value (find-sibling :multi-line-editable-text-1 widget))))))
  t)
