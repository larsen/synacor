(in-package #:synacor-gui)
(in-readtable :qtools)

(define-widget main-window (QMainWindow)
  ((cpu :initarg :cpu
        :accessor cpu
        :initform (error "CPU required"))))

(define-initializer (main-window setup)
  (signal! main-window (refresh-register-display string)
           "r1")
  (signal! main-window (update-pc int) (pc cpu))
  (signal! main-window (update-halt string)
           (if (halt cpu)
               "HALT"
               "-"))
  (dump memory-dump (cpu main-window)))
