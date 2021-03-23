;;;; package.lisp

(defpackage #:synacor
  (:use #:cl)
  (:export #:read-bin-file
           #:load!
           #:r1
           #:r2
           #:r3
           #:r4
           #:r5
           #:r6
           #:r7
           #:r8
           #:pc
           #:halt
           #:mem
           #:stack
           #:breakpoints
           #:reset-cpu!
           #:run!
           #:step!
           #:read-out-bus
           #:disassemble-instruction
           #:disassemble-instruction-at-point))

(defpackage #:synacor-gui
  (:use #:cl+qt #:synacor)
  (:export #:synacor-gui))
