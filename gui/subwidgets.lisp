(in-package #:synacor-gui)
(in-readtable :qtools)

(define-subwidget (main-window output-text-area)
    (q+:make-qtextbrowser main-window))

(define-subwidget (main-window pc-value)
    (q+:make-qlabel "0" main-window))

(define-subwidget (main-window halt-value)
    (q+:make-qlabel "-" main-window))

(define-subwidget (main-window r1-value) (q+:make-qlabel "0" main-window))
(define-subwidget (main-window r2-value) (q+:make-qlabel "0" main-window))
(define-subwidget (main-window r3-value) (q+:make-qlabel "0" main-window))
(define-subwidget (main-window r4-value) (q+:make-qlabel "0" main-window))
(define-subwidget (main-window r5-value) (q+:make-qlabel "0" main-window))
(define-subwidget (main-window r6-value) (q+:make-qlabel "0" main-window))
(define-subwidget (main-window r7-value) (q+:make-qlabel "0" main-window))
(define-subwidget (main-window r8-value) (q+:make-qlabel "0" main-window))

(define-widget instruction (QWidget)
  ((representation :initarg :representation)))

(define-subwidget (instruction instruction-representation)
    (q+:make-qlabel representation instruction))

(define-subwidget (main-window memory-dump)
    (let ((md (make-instance 'memory-dump)))
      (dump md (cpu main-window))
      md))

(define-subwidget (main-window step-button)
    (q+:make-qpushbutton "Step" main-window))

(define-subwidget (main-window run-button)
    (q+:make-qpushbutton "Run" main-window))

(define-subwidget (main-window reset-button)
    (q+:make-qpushbutton "Reset" main-window))

(define-subwidget (main-window clear-output-button)
    (q+:make-qpushbutton "Clear output area" main-window))

(define-subwidget (main-window status-bar)
    (let ((widget (q+:make-qstatusbar main-window)))
      (q+:show-message widget "Ready.")
      widget))

