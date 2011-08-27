(defpackage :avcl-forms
  (:use :cl :ol-utils :qt-utils)
  (:export :define-form
           :from
           :from-collection
           :close-form
           :form-value))

(in-package :avcl-forms)

(defqclass form-base (q-widget)
  ()
  (:signals new-data))

(defmethod initialize-instance :after ((form-base form-base) &key)
  (qt:new form-base))

;; braucht aufrufe für close-form, form-value (setf-bar)
;; Button click hat closure mit alist der felder einträge aufzurufen
(defmacro! define-form (name title parameters fields buttons &body init)
  (let ((button-syms (list->gensyms buttons))
        (field-names (mapcar #'first fields)))
   `(progn 
      (defqclass ,name (form-base)
        (,@(mapcar #`(,a1 :initarg ,(keyw a1)) parameters)
           ,@field-names                ; input fields -- these contains input WIDGETS
           ,@button-syms                ; buttons
           )
        ;; TODO make sure gensyms are fine for slot-names (should be,
        ;; as they are converted to strings)
        (:slots ,@(mapcar (lambda (s b)
                            `(,s (lambda (,g!this)
                                   (aif
                                    (funcall ,(second b)
                                             (field-values ,g!this))
                                    (qemit ,g!this new-data it)))))
                          button-syms buttons)))

      (defmethod initialize-instance
          :after ((,name ,name)
                  &key ,@(mapcar (lambda (f)
                                   (destructuring-bind (field-name type &key default) f
                                     (declare (ignorable type))
                                     `(,field-name ,default)))
                                 fields))
        (setf (q title ,name) ,title)
        (let ((,g!button-box (make-qinstance 'widget))
              (,g!form-area  (make-qinstance 'widget))
              ;; create the buttons
              ,@(mapcar #2`(,a1 (make-qinstance 'push-button ,(first a2)))
                        button-syms buttons))
          ;; hook up the buttons
          ,@(mapcar #2`(qconnect ,a1 clicked ,name ,a2) button-syms button-syms)
          ;; and put them in their box
          (qlayout ,g!button-box h-box
            ,@button-syms)
          ;; create the form fields
          (qlayout ,g!form-area form
            ,@(mapcan (lambda (f) (create-input (mkatom (second f)) name f)) fields))
          )
        
        
        ))))

(defgeneric create-input (type class-name options))

(defmacro def-create-input (type &body body)
  `(defmethod create-input ((type (eql ',type)) class-name options)
     (destructuring-bind (field-name type &key default) options
       (declare (ignorable type default))
       `(,(format nil "~:(~A~)" field-name) ; Label
          (setf (slot-value ,class-name ',field-name)
                ,(progn ,@body))))))

(def-create-input string
  `(make-instance 'string-input :default ,default))
(def-create-input integer
  `(make-instance 'integer-input :default ,default))
(def-create-input boolean
  `(make-instance 'boolean-input :default ,default))

(def-create-input from
  `(make-instance 'list-selector-input
                  :list (list ,@(cdr type))
                  :default ,default))
(def-create-input from-collection
  `(make-instance 'collection-selector-input
                  :collection ,(second type)))

(def-create-input :parameter
  `(make-instance 'display-input
                  ) ; TODO
  )

;; the various input types
(defqclass elem-input ()
  ())

(defgeneric field-value (input))
(defgeneric set-field-value (input value))
(defsetf field-value set-field-value)

(defmacro! def-elem-input (name widget value-property)
  `(progn
     (defqclass ,name (,widget elem-input)
       ())
     (defmethod field-value ((,name ,name))
       (q ,value-property ,name))
     (defmethod set-field-value ((,name ,name) ,g!value)
       (setf (q ,value-property ,name) ,g!value))))

(def-elem-input string-input q-line-edit text)
(def-elem-input integer-input q-spin-box value)

(defqclass boolean-input (q-check-box elem-input)
  ())
(defmethod field-value ((boolean-input boolean-input))
  (q is-checked boolean-input))
(defmethod set-field-value ((boolean-input boolean-input) value)
  (q set-checked boolean-input value))


(defmethod initialize-instance :after ((elem-input elem-input) &key default)
  (qt:new elem-input)
  (when default
    (setf (field-value elem-input)
          default)))

;; Beispiel für define-form Aufruf
(DEFINE-FORM TAETIGKEIT-CREATE-FORM
       "Taetigkeit anlegen"
       NIL
       ((TITEL STRING) (BEDARF INTEGER :DEFAULT 2) (DOZENT STRING)
        (BEREICH (FROM "Grundstudium" "Aufbaustudium" "Masterstudium"))
        (TERMIN STRING) (BEMERKUNG STRING))
       (("Speichern && Weiter"
         (LAMBDA (#:ALIST1048)
           (APPLY #'MAKE-INSTANCE 'TAETIGKEIT #:ALIST1048)))
        ("Speichern"
         (LAMBDA (#:ALIST1048)
           (PROG1 (APPLY #'MAKE-INSTANCE 'TAETIGKEIT #:ALIST1048)
             (CLOSE-FORM))))
        ("Abbrechen"
         (LAMBDA (#:ALIST1048)
           (DECLARE (IGNORABLE #:ALIST1048))
           (CLOSE-FORM)))))