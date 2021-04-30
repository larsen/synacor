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

(define-widget memory-dump (QTableWidget)
  ())

(define-initializer (memory-dump setup)
  (setf (q+:column-count memory-dump) 1)
  (q+:hide (q+:horizontal-header memory-dump))
  (setf (q+:stretch-last-section (q+:horizontal-header memory-dump)) T))

(defun empty-memory-dump (widget)
  (loop for i from 0 to (- (q+:row-count widget) 1)
        do (q+:remove-row widget i))
  (q+:resize-rows-to-contents widget))

(defun dump (widget cpu &key instruction-pointer)
  (let* ((pc (pc cpu))
         (instructions (synacor:disassemble-instruction-at-point
                        cpu
                        :instruction-pointer (or instruction-pointer pc)
                        :steps 1)))
    (empty-memory-dump widget)
    (dolist (instr instructions)
      (let ((tindex (q+:row-count widget)))
        (q+:insert-row widget tindex)
        (setf (q+:cell-widget widget tindex 0)
              (make-instance 'instruction :representation instr))
        (q+:resize-rows-to-contents widget)))))

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

