(defpackage :avcl-models
  (:use :cl :ol-utils :qt-utils
        :avcl-forms :avcl-views :avcl-storage)
  (:export :persistable :persistent-id
           :defclass/q
           :taetigkeit :titel :bedarf :dozent :bereich :termin :bemerkung
           :assistent :name
           :wunsch :staerke
           :zuweisung :score :fest
           :wuensche
           :zuweisungen))

(in-package :avcl-models)

(defgeneric equals (a b))
(defmethod equals (a b)
  (equal a b))

(defclass persistable ()
  ((id :accessor persistent-id)))

(defmacro! defclass/q (name slots print-form)
  (let ((slot-names (mapcar #'first slots)))
    `(progn
       (defclass ,name (persistable)
         ,(mapcar (lambda (slot)
                    (destructuring-bind (slot-name slot-type &key (default nil default-p)) slot
                      (declare (ignorable slot-type))
                      `(,slot-name :initarg ,(keyw slot-name)
                                   :accessor ,slot-name
                                   ,@(if default-p `(:initform ,default)))))
                  slots))
       (defmethod print-object ((,g!object ,name) ,g!stream)
         (print-unreadable-object (,g!object ,g!stream :type t)
           (with-slots ,slot-names ,g!object
             (declare (ignorable ,@slot-names))
             (format ,g!stream ,@print-form))))
       ,(generate-create-form name slots slot-names)
       ,(generate-edit-form name slots slot-names)
       ,(generate-list-model name slot-names))))

(ew
  (defun generate-create-form (name slots slot-names)
    (let ((parameter-slots (filter (lambda (slot) (if (member :parameter slot) (first slot))) slots)))
      (with-gensyms!
        `(define-form ,(symb name '-create-form) ,(format nil "~:(~A~) anlegen" name)
             (,@parameter-slots)        ; Parameter
             ,slots                     ; Felder
             ;; Buttons
             (("Speichern && Weiter" (lambda (,g!alist)
                                       (prog1
                                           #1=(make-instance ',name
                                                             ,@(mapcan #`(,(keyw a1) (assoc1 ',a1 ,g!alist))
                                                                       slot-names))
                                           (clear-form))))
              ("Speichern" (lambda (,g!alist)
                             (prog1
                                 #1#
                               (close-form))))
              ("Abbrechen" (lambda (,g!alist)
                             (declare (ignorable ,g!alist))
                             (close-form))))
           ;; keine Initialisierung
           ))))

  (defun generate-edit-form (name slots slot-names)
    (with-gensyms!
      `(define-form ,(symb name '-edit-form) ,(format nil "~:(~A~) bearbeiten" name)
           (,name)                      ; Parameter
           ,slots                       ; Felder
           ;; Buttons
           (("Speichern" (lambda (,g!alist)
                           (let ((,g!object (assoc1 ',name ,g!alist)))
                             ,@(mapcar (lambda (slot-name)
                                         `(setf (slot-value ,g!object ',slot-name)
                                                (assoc1 ',slot-name ,g!alist)))
                                       slot-names)
                             (close-form))))
            ("Abbrechen" (lambda (,g!alist)
                           (declare (ignorable ,g!alist))
                           (close-form))))
         ;; Initialisierung
         (let ((,name (form-value ',name)))
           ,@(mapcar (lambda (slot-name)
                       `(setf (form-value ',slot-name)
                              (slot-value ,name ',slot-name)))
                     slot-names)))))

  (defun generate-list-model (name slot-names)
    `(define-tree-model ,(symb name '-list-model)
         ,(mapcar (lambda (s) (string-capitalize (symbol-name s))) slot-names) ; header
       (lambda (root) (funcall root)) ,(mapcan #`(#',a1) slot-names))))

(defclass/q taetigkeit
    ((titel string)
     (bedarf integer :default 2)
     (dozent string)
     (bereich (from "Grundstudium"
                    "Aufbaustudium"
                    "Masterstudium"))
     #|(termin string)|#
     #|(bemerkung string)|#)
  ("~A : ~A [~A]" titel dozent bedarf))

(defclass/q assistent
    ((name string)
     (bedarf integer :default 1))
  ("~A" name))

(defclass/q wunsch
    ((assistent :parameter)
     (taetigkeit (from-list (->list (collection :taetigkeiten))))
     (staerke (from 1 2 3 4)))
  ("~A wuenscht ~A [W~A]" (name assistent) (titel taetigkeit) staerke))

(defclass/q zuweisung
    ((assistent  (from-list (->list (collection :assistenten))))
     (taetigkeit (from-list (->list (collection :taetigkeiten))))
     (score integer :default 0)
     (fest boolean  :default nil))
  ("~A uebernimmt ~A~:[~; [fest]~]" (name assistent) (titel taetigkeit) fest))

(defmacro! def-filter-method (class collection &optional (accessor class))
  `(defmethod ,collection ((,class ,class))
     (remove-if-not (lambda (,g!x)
                      (eql ,g!x ,class))
                    (->list (collection ,(keyw collection)))
                    :key #',accessor)))

(def-filter-method  assistent   wuensche)
(def-filter-method  taetigkeit  wuensche)
(def-filter-method  assistent   zuweisungen)
(def-filter-method  taetigkeit  zuweisungen)

(defmacro define-equals-from-slots (class &rest slots)
  `(defmethod equals ((a ,class) (b ,class))
     (and ,@(mapcar #`(equals (slot-value a ',a1)
                              (slot-value b ',a1))
                    slots))))

;;; comparison for use with collection
(define-equals-from-slots taetigkeit  titel dozent)
(define-equals-from-slots assistent  name)
(define-equals-from-slots wunsch  assistent taetigkeit)

(defmethod equals ((a zuweisung) (b zuweisung))
  (eql a b))

(defun show-list-model (model-name collection)
  (let ((model (make-instance model-name
                              :root (lambda ()
                                      (->list (collection collection)))))
        (view  (make-qinstance 'tree-view)))
    (setf (q model view) model)
    (fetch model)
    (q show view)))
