(defpackage :avcl-forms
  (:use :cl :ol-utils :qt-utils)
  (:export :define-form
           :from
           :from-list
           :close-form
           :form-value))

(in-package :avcl-forms)

(defqclass form-base (q-widget)
  ((data-queue :accessor data-queue
               :initform (make-queue)))
  (:signals new-data))

(defmethod initialize-instance :after ((form-base form-base) &key)
  (qt:new form-base))

(defmethod next-form-datum ((form-base form-base))
  (dequeue (data-queue form-base)))

(defmethod form-data ((form-base form-base))
  (car (data-queue form-base)))

(defgeneric field-values (form))

;; braucht aufrufe für close-form, form-value (setf-bar)
;; Button click hat closure mit alist der felder einträge aufzurufen
(defmacro! define-form (name title parameters fields buttons &body init)
  (let* ((button-syms     (list->gensyms buttons))
        (field-names      (mapcar #'first fields))
        (effective-fields (set-difference field-names parameters)))
   `(progn 
      (defqclass ,name (form-base)
        (,@(mapcar #`(,a1 :initarg ,(keyw a1)) parameters)
           ,@effective-fields ; input fields -- these contains input WIDGETS
           ,@button-syms                ; buttons
           )
        ;; TODO make sure gensyms are fine for slot-names (should be,
        ;; as they are converted to strings)
        (:slots ,@(mapcar (lambda (s b)
                            `(,s (lambda (,g!this)
                                   (labels ((close-form ()
                                              (q close ,g!this))
                                            (clear-form ()
                                              (clear-values ,g!this)))
                                    (aif
                                     (funcall ,(second b)
                                              (field-values ,g!this))
                                     (progn
                                       (enqueue it (data-queue ,g!this))
                                       (qemit ,g!this new-data)))))))
                          button-syms buttons)))

      (defmethod initialize-instance
          :after ((,name ,name)
                  &key ,@(mapcar (lambda (f)
                                   (destructuring-bind (field-name type &key default) f
                                     (declare (ignorable type))
                                     `(,field-name ,default)))
                                 fields))
        (declare (ignorable ,@field-names))
        (setf (q window-title ,name) ,title)
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
          ;; put button box and form into this dialog
          (qlayout ,name v-box
            ,g!form-area
            ,g!button-box))
        ;; run the custom initialisation code
        (macrolet ((form-value (,g!id)
                     (if (member ,g!id ',parameters)
                         `(slot-value ,',name ,,g!id)
                         `(field-value (slot-value ,',name ,,g!id)))))
          ,@init))
      (defmethod field-values ((,name ,name))
        (list ,@(mapcar #`(cons ',a1 (slot-value ,name ',a1))
                        parameters) ; first put the parameters into the alist
              ,@(mapcar #`(cons ',a1 (field-value (slot-value ,name ',a1)))
                        effective-fields) ; then the field values
              ))
      (defmethod clear-values ((,name ,name))
        ,@(mapcar #`(clear-field-value (slot-value ,name ',a1))
                  effective-fields)))))

(defgeneric create-input (type class-name options))

(defmacro def-create-input (type &body body)
  `(defmethod create-input ((type (eql ',type)) class-name options)
     (destructuring-bind (field-name type &key default) options
       (declare (ignorable type default))
       (unless (eql type :parameter)  ; no need for parameter inputs
           `(,(format nil "~:(~A~)" field-name) ; Label
           (setf (slot-value ,class-name ',field-name)
                 ,(progn ,@body)))))))

(def-create-input string
  `(make-instance 'string-input :default ,default))
(def-create-input integer
  `(make-instance 'integer-input :default ,default))
(def-create-input boolean
  `(make-instance 'boolean-input :default ,default))

(def-create-input from
  `(make-instance 'list-selector-input
                  :list (list ,@(cdr type))))
(def-create-input from-list
  `(make-instance 'list-selector-input
                  :list ,(second type)))

(def-create-input :parameter)

;; the various input types
(defgeneric field-value (input))
(defgeneric set-field-value (input value))
(defsetf field-value set-field-value)
(defgeneric clear-field-value (input))

(defsymconstant +no-default+)

(defmacro! def-elem-input (name widget value-property
                                &key boolean-property)
  `(progn
     (defqclass ,name (,widget)
       ((default :initarg :default :initform +no-default+)))
     (defmethod initialize-instance :after ((,name ,name) &key)
       (qt:new ,name)
       (clear-field-value ,name))
     (defmethod field-value ((,name ,name))
       (q ,(if boolean-property
               (symb 'is- value-property)
               value-property) ,name))
     (defmethod set-field-value ((,name ,name) ,g!value)
       (setf (q ,value-property ,name) ,g!value))
     (defmethod clear-field-value ((,name ,name))
       (unless (eq (slot-value ,name 'default) +no-default+)
         (setf (field-value ,name)
               (slot-value ,name 'default))))))

(def-elem-input string-input q-line-edit text)
(def-elem-input integer-input q-spin-box value)
(def-elem-input boolean-input q-check-box checked :boolean-property t)


(defqclass list-selector-input (q-combo-box)
  ((list :initarg :list :initform nil)))

(defmethod initialize-instance :after ((list-selector-input list-selector-input) &key)
  (qt:new list-selector-input)
  ;; fill the combo box with the given choices
  (dolist (l (slot-value list-selector-input 'list))
    (q add-item list-selector-input
       (with-output-to-string (str)
         (princ l str)))))

(defmethod field-value ((list-selector-input list-selector-input))
  (let ((index (q current-index list-selector-input)))
    (unless (minusp index)
      (elt (slot-value list-selector-input 'list) index))))

(defmethod set-field-value ((list-selector-input list-selector-input) value)
  (let ((index (or (position value (slot-value list-selector-input 'list))
                   -1)))
    (setf (q current-index list-selector-input) index)))

(defmethod clear-field-value ((list-selector-input list-selector-input))
  (setf (q current-index list-selector-input) -1))
