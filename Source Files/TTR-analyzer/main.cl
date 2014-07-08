
(in-package :common-graphics-user)

(defclass TTR_tool (dialog) 
  ((measure :accessor measure :initform "ttr")
   (texts :accessor texts :initform ())
   (struct :accessor struct :initform ())
   (text-count :accessor text-count :initform 0))
  )

(defstruct (Text)
  id
  tokenized
  stemmer
  ttr
  ttr_list
  kgm
  mtld
  )

;;;;;;;;;;;;;;;;;;
;;;;Menu-Items;;;;
;;;;;;;;;;;;;;;;;;

(defun set_ttr(dialog)
  (setf (slot-value dialog 'measure) "ttr"
    (selected(find-named-object 'simple_ttr (menu dialog))) t
    (selected(find-named-object 'kgm (menu dialog))) nil)
  )

(defun set_kgm(dialog)
  (setf (slot-value dialog 'measure) "kgm"
    (selected(find-named-object 'kgm (menu dialog))) t
    (selected(find-named-object 'simple_ttr (menu dialog))) nil)
  )

(defun contact(dialog)
  (let ((contact_info (make-window :modal-dialog-test
                        :owner dialog
                        :title "Contact"
                        :pop-up t
                        :height 200
                        :width 340
                        :dialog-items 
                        (list (make-instance 'static-text
                                :height 500
                                :width 340
                                :value "For more informations visit https://github.com/arbox/cldh-projektseminar-2013 or contact me directly: lehmler@gmx.de"))
                 )))
  (pop-up-modal-dialog contact_info))
  )

(defun help(dialog)
  (let ((contact_info (make-window :modal-dialog-test
                        :owner dialog
                        :title "Guide"
                        :pop-up t
                        :height 500
                        :width 400
                        :dialog-items 
                        (list (make-instance 'static-text
                                :height 500
                                :width 340
                                :value (file-contents "C:\\Users\\Stephan\\Documents\\Studium\\5. Semester\\Projektseminar\\TTR-analyzer\\guide.txt")))
                 ))) 
  (pop-up-modal-dialog contact_info))
  )

(defun text_laden(form)
  (let ((datei (ask-user-for-existing-pathname "Datei laden"))
        )
    (when datei
      (progn

        (incf (slot-value form 'text-count))

        (let 
            ((x (make-Text)))
          (setf (Text-id x) (slot-value form 'text-count)
                 (Text-stemmer x) (ask-user-for-choice-from-list "Choose Stemmer" (list "none" "english" "german" "russian")))
          
          (if (equal "russian" (Text-stemmer x))
              (setf (slot-value form 'texts) (append (slot-value form 'texts) (list (file-contents datei :external-format :utf-8))))
              (setf (slot-value form 'texts) (append (slot-value form 'texts) (list (file-contents datei))))
          )
          
          (setf 
           (value(find-component :static-text-4 form))
                  "Tokenizing")
          
          (if (equal (Text-stemmer x) "none") 
              (setf (Text-tokenized x) (naive_tokenizer (nth (- (slot-value form 'text-count) 1)(slot-value form 'texts))))
            (if (equal (Text-stemmer x) "english")
                (setf (Text-tokenized x) (iterate_english (naive_tokenizer (nth (- (slot-value form 'text-count) 1)(slot-value form 'texts)))))
              (if (equal (Text-stemmer x) "german")
                  (setf (Text-tokenized x) (iterate_german(naive_tokenizer(nth (- (slot-value form 'text-count) 1)(slot-value form 'texts)))))
                (if (equal (Text-stemmer x) "russian")
                    (setf (Text-tokenized x) (iterate_russian (naive_tokenizer (nth (- (slot-value form 'text-count) 1)(slot-value form 'texts))))))
                )
              )
            )

          
          (setf 
           (value(find-component :static-text-4 form))
           "Calculating TTR")
          
          (setf (Text-ttr_list x) (list_TTR (Text-tokenized x))
                (Text-ttr x) (first(last (Text-ttr_list x)))
            (slot-value form 'struct) (append (slot-value form 'struct) (list x)))
          
          
          (setf (value(find-component :static-text-4 form)) "Calculating KGM"
                (Text-kgm x) (kgm x))
        
          
        (let 
            ((entry (write-to-string (Text-id x)))
             (text x))

        (add-item 
           (find-named-object :text-view form) 
           (make-instance 'list-view-item
             :name (intern entry)
             :value-plist `(:number ,entry 
                            :ttr ,(Text-ttr text)
                            :kgm ,(car(last(kgm text)))
                            :mtld ,(mtld (Text-tokenized text))
                            :stemmer ,(Text-stemmer text)))
        )
              
      (add-tab (find-named-object :tab-control form) (intern entry) entry)
      (add-component-to-tab (find-named-object :tab-control form)
                        (intern entry)
                        (make-instance 'multi-line-editable-text
                          :bottom-attachment :scale
                          :left-attachment :scale
                          :right-attachment :scale
                          :height 432
                          :width 790
                          :font
                          (make-font-ex nil "Segoe UI / Default" 15)
                          :left 0
                          :name (intern (concatenate 'string "Text-" entry))
                          :top 36
                          :value  (nth (- (slot-value form 'text-count) 1)(slot-value form 'texts))))
       (setf 
           (value(find-component :static-text-4 form))
           "ready..")
          )
       )
      )
   )
  )
  )

(defun save_graph(form)
  (let ((path (ask-user-for-new-pathname "Speichern" :allowed-types (list(cons "Pixmap Files" "*.bmp")))))
    (when path
      (save-pixmap (get-pixmap (window(find-named-object :chart-widget-1 form))) path)))
  )

(defun main-tab-control-on-double-click (dialog widget)
  (declare (ignorable dialog widget)) 
  (unless
      (or (eq (value (find-sibling :tab-control widget)) :start) (eq (value (find-sibling :tab-control widget)) :graph))
    (progn
      (print (value (find-sibling :tab-control widget)))
      (setf (slot-value dialog 'texts) (delete (nth ( - (read-from-string(symbol-name (value (find-sibling :tab-control widget)))) 1) (slot-value dialog 'texts)) (slot-value dialog 'texts)))
      (setf (slot-value dialog 'struct) (delete (nth ( - (read-from-string(symbol-name (value (find-sibling :tab-control widget)))) 1) (slot-value dialog 'struct)) (slot-value dialog 'struct)))
      (remove-item (find-named-object :text-view dialog) (value (find-sibling :tab-control widget))) 
      (remove-tab (find-sibling :tab-control widget) (value (find-sibling :tab-control widget)))
      (decf (slot-value dialog 'text-count))))
  t)

(defun main-draw-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  (let ((l ()))
    (if (string= (slot-value dialog 'measure) "ttr")
        
       ;;;if measure is ttr 
        
      (progn
          (setf (title(find-sibling :chart-widget-1 widget)) "TTR-Chart")
        (dolist 
        (x (slot-value dialog 'struct)) 
      (plot-TTR (find-sibling :chart-widget-1 widget) x)
      (setf l (append l (list (list :id (Text-id x)))))
      ))
      
      ;;;if measure is kgm
      
      (progn
          (setf (title(find-sibling :chart-widget-1 widget)) "KGM-Chart")
      (dolist
          (x (slot-value dialog 'struct))
      (plot-KGM (find-sibling :chart-widget-1 widget) x)
      (setf l (append l (list (list :id (Text-id x)))))
        ))
      ) 
    (setf (chart-objects (find-sibling :chart-widget-1 widget)) (apply #'vector  l))
    )
  (setf (value (find-sibling :tab-control widget)) :Graph)
  t)

(defun main-compare-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  (when (= 2 (length (value(find-sibling :text-view widget))))
    (progn
      (let* 
          ((x (read-from-string(symbol-name (first (value(find-sibling :text-view widget))))))
           (y (read-from-string(symbol-name (second (value(find-sibling :text-view widget))))))
           (textx (nth (- x 1) (slot-value dialog 'struct)))
           (texty (nth (- y 1) (slot-value dialog 'struct)))
           )
    (setf 
     (value(find-sibling :static-text-2 widget)) 
     (concatenate 'string "Spearman's Roh for Text " (write-to-string x) " and Text " (write-to-string y) " is: " (write-to-string(compute_roh textx texty)))
     ))))
  t)

