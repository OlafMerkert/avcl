(defpackage :avcl-storage
  (:use :cl :ol-utils :cl-prevalence)
  (:export :*default-store*
           :load-data :save-data
           :collection
           :->list
           :add :del :map*))

(in-package :avcl-storage)

;;; Benutze cl-prevalence, um daten zu speichern
;; (cl-prevalence:find-all-objects *default-store* 'assistent)

(defparameter *storage-dir* #P"~/.avcl/data/")

(defparameter *default-store* nil)

(defun load-data ()
  (setf *default-store*
        (make-prevalence-system *storage-dir*)))
(defun save-data ()
  (snapshot *default-store*))

;; TODO collection für die Vorhaltung der ganzen Listen
(defclass collection ()
  ((data-array :initarg :data
               :initform (make-array 100 :adjustable t :fill-pointer 0)
               :accessor data)))

(defgeneric ->list (collection))
(defmethod ->list ((collection collection))
  (array->list (data collection)))

(defgeneric add (item collection))
(defmethod add (item (collection collection))
  (vector-push-extend item (data collection)))

(defgeneric del (item collection))
(defmethod del (item (collection collection))
  (setf #1=(data collection)
        (delete item #1#)))

(defgeneric map* (result-type fn collection))
(defmethod map* (result-type fn (collection collection))
  (map result-type fn (data collection)))

(defun collection (keyword)
  (or #1=(get-root-object *default-store* keyword)
      (values (setf #1#
                    (make-instance 'collection))
              t)))
