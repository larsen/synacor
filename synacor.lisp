;;;; synacor.lisp

(in-package #:synacor)

(defvar *cpu* nil)

(defun test-machine (&key (with-instruction-log nil))
  (setf *cpu* (make-instance 'cpu))
  (reset-cpu! *cpu*)
  (load! (read-bin-file *challenge-bin*) *cpu*)
  ;; Why I had to add this?
  (set-address! 865 844 *cpu*)
  (run-cpu-in-console *cpu*))

(defun run-cpu-in-console (cpu)
  (run! cpu
        :callback (lambda ()
                    (let ((ch (synacor:read-out-bus cpu)))
                      (when ch
                        (princ (code-char ch)))))))
