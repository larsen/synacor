(in-package #:synacor-gui)
(in-readtable :qtools)

(define-subwidget (main-window output-text-area)
    (q+:make-qtextbrowser main-window))

(define-subwidget (main-window pc-value)
    (q+:make-qlabel "0" main-window))

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
    (let* ((md (q+:make-qtablewidget main-window))
           (cpu (cpu main-window))
           (pc (pc cpu))
           (instructions (synacor:disassemble-instruction-at-point cpu
                          :instruction-pointer pc
                          :steps 10)))
      (setf (q+:column-count md) 1)
      (dolist (instr instructions)
        (let ((tindex (q+:row-count md)))
          (q+:insert-row md tindex)
          (setf (q+:cell-widget md tindex 0)
                (make-instance 'instruction :representation instr))))
      (q+:resize-rows-to-contents md)
      (setf (q+:stretch-last-section (q+:horizontal-header md)) T)
      (q+:hide (q+:horizontal-header md))
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

