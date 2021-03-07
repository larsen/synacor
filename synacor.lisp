;;;; synacor.lisp

(in-package #:synacor)

(defun test-machine (&key (with-instruction-log nil))
  (setf *cpu* (make-instance 'cpu))
  (reset-cpu! *cpu*)
  (load! (read-bin-file *challenge-bin*) *cpu*)
  ;; Why I had to add this?
  (set-address! 865 844 *cpu*)
  (run! *cpu* :with-instruction-log with-instruction-log))
