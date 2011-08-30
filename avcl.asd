(defsystem avcl
  :serial t
  :depends-on ("ol-utils" "qt-utils" "qt" "cl-prevalence")
  :components ((:module "ui"
                        :serial t
                        :components ((:file "forms")
                                     (:file "views")))
               (:file "storage")
               (:file "models")
               (:file "entry")
               (:file "assign")
               (:file "playground")))
