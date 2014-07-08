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