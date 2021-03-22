(in-package #:synacor-gui)
(in-readtable :qtools)

(define-widget main-window (QMainWindow)
  ((cpu :initarg :cpu
        :accessor cpu
        :initform (error "CPU required"))))
