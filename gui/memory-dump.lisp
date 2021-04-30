(in-package #:synacor-gui)
(in-readtable :qtools)


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
                        :steps 20)))
    (empty-memory-dump widget)
    (dolist (instr instructions)
      (let ((tindex (q+:row-count widget)))
        (q+:insert-row widget tindex)
        (setf (q+:cell-widget widget tindex 0)
              (make-instance 'instruction :representation instr))
        (q+:resize-rows-to-contents widget)))))
