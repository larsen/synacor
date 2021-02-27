;;;; synacor.asd

(asdf:defsystem #:synacor
  :description "Synacor challenge"
  :author "Stefano Rodighiero"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "cpu")
               (:file "binformat")
               (:file "synacor")))
