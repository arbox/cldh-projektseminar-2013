(in-package :common-graphics-user)

(defclass TTR_tool (dialog) 
  ((texts :accessor texts :initform ())
   (struct :accessor struct :initform ())
   (text-count :accessor text-count :initform 0))
  )

;;;Structure for each Text
;;;Contains calculated Data
(defstruct (Text)
  id
  tokenized
  stemmer
  ttr
  ttr_list
  kgm
  mtld
  )


;;;Loads Text into the program
;;;creates a Text-structure and calculates its id, tokenized, stemmer, ttr etc.
;;;this may take a while...
;;;updates list-view and tab-control

(defun text_laden(form)
  (let ((datei (ask-user-for-existing-pathname "Datei laden"))
        )
    (when datei
      (progn
        
        ;;;set name of text for tab, increments text-count
        (let ((name (string-downcase (pathname-name datei))))  
        (incf (slot-value form 'text-count))
          
          ;;;creates x = Text-structure
          ;;;sets id and stemmer
        (let 
            ((x (make-Text)))
          (setf (Text-id x) (slot-value form 'text-count)
                 (Text-stemmer x) (ask-user-for-choice-from-list "Choose Stemmer" (list "none" "english" "german" "russian")))
          
          ;;;Loads text into tab
          ;;;if the text is cyrillic, change to utf-8
          (if (equal "russian" (Text-stemmer x))
              (setf (slot-value form 'texts) (append (slot-value form 'texts) (list (file-contents datei :external-format :utf-8))))
              (setf (slot-value form 'texts) (append (slot-value form 'texts) (list (file-contents datei))))
          )
          
          
          ;;;Start Tokenizing
          ;;;apply chosen Stemmer
          
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
          
          ;;;error-handling
          ;;;not choosing any stemmer or using russian stemmer for non-cyrillic texts
          
          (if (or (not (Text-tokenized x))(equal "" (car(Text-tokenized x))))
              (progn
                (unless (not (Text-tokenized x))
                        (setf (value(find-component :warning form)) "Don't use the russian stemmer, unless text is in Cyrillic script"
                              (value(find-component :static-text-4 form)) ""))
                (decf (slot-value form 'text-count))
                (setf (slot-value form 'texts) (delete (nth (slot-value form 'text-count) (slot-value form 'texts)) (slot-value form 'texts)))
                )
            
            ;;; if no error, then start calculating values
            ;;;TTR,KGM,MTLD
            (progn
              (setf (value(find-component :warning form)) "")
              (setf (value(find-component :static-text-4 form)) "Calculating TTR")
              (setf (Text-ttr_list x) (list_TTR (Text-tokenized x))
                    (Text-ttr x) (first(last (Text-ttr_list x)))
                    (slot-value form 'struct) (append (slot-value form 'struct) (list x)))
              (setf (value(find-component :static-text-4 form)) "Calculating KGM"
                (Text-kgm x) (kgm x)
                (value(find-component :static-text-4 form))  "Calculating MTLD"
                (Text-mtld x) (mtld (Text-tokenized x)))
              
              ;;;Add Text to list-view
              
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
                            :mtld ,(Text-mtld text)
                            :stemmer ,(Text-stemmer text)))
                 )
                
                ;;;Add Text to Tab-control

               (add-tab (find-named-object :tab-control form) (intern entry) name)
               (add-component-to-tab (find-named-object :tab-control form)
                                     (intern entry)
                                     (make-instance 'multi-line-editable-text
                                         :bottom-attachment :scale
                                         :left-attachment :scale
                                         :right-attachment :scale
                                         :height 432
                                         :width 790
                                         :font (make-font-ex nil "Segoe UI / Default" 15)
                                         :left 0
                                         :name (intern (concatenate 'string "Text-" entry))
                                         :top 36
                                         :value  (nth (- (slot-value form 'text-count) 1)(slot-value form 'texts))))
                )
              (setf (value(find-component :static-text-4 form)) "ready..")
          )
       )
      )
   )
  )
    )
  )
)

;;;removes a Tab (unless its "Graph" or "Start") from Tab-view
;;;Deletes any traces (list-view, count, content...)

(defun main-tab-control-on-double-click (dialog widget)
  (declare (ignorable dialog widget)) 
  (unless
      (or (eq (value (find-sibling :tab-control widget)) :start) (eq (value (find-sibling :tab-control widget)) :graph))
    (progn
      (setf (slot-value dialog 'texts) (delete (nth ( - (read-from-string(symbol-name (value (find-sibling :tab-control widget)))) 1) (slot-value dialog 'texts)) (slot-value dialog 'texts)))
      (setf (slot-value dialog 'struct) (delete (nth ( - (read-from-string(symbol-name (value (find-sibling :tab-control widget)))) 1) (slot-value dialog 'struct)) (slot-value dialog 'struct)))
      (remove-item (find-named-object :text-view dialog) (value (find-sibling :tab-control widget))) 
      (remove-tab (find-sibling :tab-control widget) (value (find-sibling :tab-control widget)))
      (decf (slot-value dialog 'text-count))))
  t)

;;;Plotts the Graph
;;;throws an error if no Text are uploaded

(defun main-draw-on-click (dialog widget)
  (declare (ignorable dialog widget))
  (if (range (find-sibling :text-view widget))
   (let ((l ()) (chart (find-sibling :chart-widget-1 widget)))
    (setf (value(find-sibling :warning widget)) "")
    (if (string= (ask-user-for-choice-from-list "Choose Type of Graph. What measure should be used?" (list "ttr" "kgm")) "ttr")
        
       ;;;if measure is ttr 
        
      (progn
        (setf (title chart) "TTR-Chart")
        (setf (axis-label (value-axis chart)) "TTR")
        (setf (axis-label (item-axis chart)) "Words")
        (dolist 
        (x (slot-value dialog 'struct)) 
      (plot-TTR chart x)
          (setf l (append l (list (list :id (Text-id x)))))
      ))
      
      ;;;if measure is kgm
      
      (progn
        (setf (title chart) "KGM-Chart")
        (setf (axis-label (value-axis chart)) "KGM")
        (setf (axis-label (item-axis chart)) "Words")
      (dolist
          (x (slot-value dialog 'struct))
      (plot-KGM chart x)
      (setf l (append l (list (list :id (Text-id x)))))
        ))
      ) 
    (setf (chart-objects chart) (apply #'vector  l))
    (setf (value (find-sibling :tab-control widget)) :graph)
    )
    (setf (value(find-sibling :warning widget)) "You didn't upload any files to analyze"))
  t)

;;;as long as exactly two Texts from list-view are selected, it displays the Spearmanns roh
;;;If none, or more than two are selected, it throws an error

(defun main-compare-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  (if (= 2 (length (value(find-sibling :text-view widget))))
      (progn
        (setf (value(find-sibling :warning widget)) "")
      (let* 
          ((x (read-from-string(symbol-name (first (value(find-sibling :text-view widget))))))
           (y (read-from-string(symbol-name (second (value(find-sibling :text-view widget))))))
           (textx (nth (- x 1) (slot-value dialog 'struct)))
           (texty (nth (- y 1) (slot-value dialog 'struct)))
           )
    (setf 
     (value(find-sibling :static-text-2 widget)) 
     (concatenate 'string "Spearman's Roh for Text " (write-to-string x) " and Text " (write-to-string y) " is: " (write-to-string(compute_roh textx texty)))
     )))
    (setf (value(find-sibling :warning widget)) "You have to select exactly 2 Texts from the list-view")
    )
  t)

;;;another way to load a Text

(defun main-add_Text-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  (text_laden dialog)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Help-Items;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Creates Contact and Guide Windows;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
                                :value "For more informations visit https://github.com/arbox/cldh-projektseminar-2013
 or contact me directly:
lehmler@gmx.de"))
                 )))
  (pop-up-modal-dialog contact_info))
  )

(defun help(dialog)
  (let ((contact_info (make-window :modal-dialog-test
                        :owner dialog
                        :title "Guide"
                        :pop-up t
                        :height 650
                        :width 400
                        :dialog-items 
                        (list (make-instance 'static-text
                                :height 600
                                :width 340
                                :value 
"*CONCEPTS*
TTR: 
Relation between Wordtokens and Wordtypes.

Simple_TTR:
Easy method to calculate the TTR. It divides the total typecount with the number of tokens. The longer a text gets, the smaller will be the TTR.

MTLD: 
Measure of textual lexical diversity. Another measure for Type-Token-Ratio. Does work better with long Texts.

KGM: 
Short for Köhler-Galle-Method, another measure. Considers the length and dynamic of a text.

Stemming:
Rule-based, automatic reduction of inflected words to their stem.
This programm offers implementations of the Porter-Stemmer for english, german or russian texts.

*TOOL*
Menu:
File:
Add Text
Save Graph-Image : Creates a bitmap-screenshot of the TTR/KGM-Graph. Only captures the area, that is actually visible
Exit: Ends Program

Buttons:
Draw-Graph: Fills the Graph-widget inside the Tab 'Graph'.
Compare: Only usable, when exactly two texts are selected from the list-view. Compares the similarity of these two texts
"))
                 ))) 
  (pop-up-modal-dialog contact_info))
  )

(defun save_graph(form)
  (let ((path (ask-user-for-new-pathname "Speichern" :allowed-types (list(cons "Pixmap Files" "*.bmp")))))
    (when path
      (save-pixmap (get-pixmap (window(find-named-object :chart-widget-1 form))) path)))
  )  