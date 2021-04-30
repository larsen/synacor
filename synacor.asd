;;;; synacor.asd

(asdf:defsystem #:synacor
  :description "Synacor challenge"
  :author "Stefano Rodighiero"
  :version "0.0.1"
  :serial t
  :depends-on (#:qtools #:qtcore #:qtgui)
  :components ((:file "package")
               (:file "cpu")
               (:file "instruction-set")
               (:file "binformat")
               (:file "synacor")
               (:module "gui"
                :components ((:file "main-window")
                             (:file "memory-dump")
                             (:file "subwidgets")
                             (:file "layout")
                             (:file "menu")
                             (:file "gui")))))
