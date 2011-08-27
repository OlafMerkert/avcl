(defpackage :avcl-models
  (:use :cl :ol-utils)
  (:export :persistable :persistent-id
           :defclass/q
           :taetigkeit :titel :bedarf :dozent :bereich :termin :bemerkung
           :assistent :name
           :wunsch :staerke
           :zuweisung :score :fest))

(in-package :avcl-models)

;; TODO collection für die Vorhaltung der ganzen Listen
(defclass collection ()
  ())

(defclass persistable ()
  ((id :accessor persistent-id)))

(defmacro! defclass/q (name slots
                      print-form)
  (let ((slot-names (mapcar #'first slots)))
   `(progn
      (defclass ,name (persistable)
        ,(mapcar (lambda (slot)
                   (destructuring-bind (slot-name slot-type &key default) slot
                     (declare (ignorable slot-type))
                     `(,slot-name :initarg ,(keyw slot-name)
                                  :accessor ,slot-name
                                  ,@(if default `(:initform ,default)))))
                 slots))
      (defmethod print-object ((,g!object ,name) ,g!stream)
        (print-unreadable-object (,g!object ,g!stream :type t)
          (with-slots ,slot-names ,g!object
            (declare (ignorable ,@slot-names))
            ,@(mapcar #`(princ ,a1 ,g!stream) print-form))))
      ,(generate-create-form name slots)
      ,(generate-edit-form name slots slot-names))))

(defun generate-create-form (name slots)
  (let ((parameter-slots (filter (lambda (slot) (if (member :parameter slot) (first slot))) slots)))
   (with-gensyms!
     `(define-form ,(symb name '-create-form) ,(format nil "~:(~A~) anlegen" name)
          (,@parameter-slots)           ; Parameter
        ,slots                          ; Felder
        ;; Buttons
        (("Speichern && Weiter" (lambda (,g!alist)
                                  (apply #'make-instance ',name ,g!alist)))
         ("Speichern" (lambda (,g!alist)
                        (prog1
                            (apply #'make-instance ',name ,g!alist)
                          (close-form))))
         ("Abbrechen" (lambda (,g!alist)
                        (declare (ignorable ,g!alist))
                        (close-form))))
        ;; keine Initialisierung
        ))))

(defun generate-edit-form (name slots slot-names)
  (with-gensyms!
    `(define-form ,(symb name '-edit-form) ,(format nil "~:(~A~) bearbeiten" name)
       (,name)                    ; Parameter
         ,slots                         ; Felder
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
         ,@(mapcar (lambda (slot-name)
                     `(setf (form-value ',slot-name)
                            (slot-value ,name ',slot-name)))
                   slot-names))))

(defclass/q taetigkeit
    ((titel string)
     (bedarf integer :default 2)
     (dozent string)
     (bereich (from "Grundstudium"
                    "Aufbaustudium"
                    "Masterstudium"))
     (termin string)
     (bemerkung string))
  (titel " : " dozent " [" bedarf "]"))

(defclass/q assistent
    ((name string)
     (bedarf integer :default 1))
  (name))

(defclass/q wunsch
    ((assistent :parameter)
     (taetigkeit (from-collection :taetigkeiten))
     (staerke (from 1 2 3 4)))
  ((name assistent) " wuenscht " (titel taetigkeit) " [W" staerke "]"))

(defclass/q zuweisung
    ((assistent  (from-collection :assistenten))
     (taetigkeit (from-collection :taetigkeiten))
     (score integer :default 0)
     (fest boolean  :default nil))
  ((name assistent) " uebernimmt " (titel taetigkeit) (if fest " [fest]" "")))
