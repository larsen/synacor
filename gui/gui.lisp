(in-package #:synacor-gui)
(in-readtable :qtools)

(define-signal (main-window update-pc) (int))

(define-slot (main-window update-pc) ((new-pc-value int))
  (declare (connected main-window (update-pc int)))
  (setf (q+:text pc-value) (format nil "~a" new-pc-value)))


(define-signal (main-window update-halt) (string))

(define-slot (main-window update-halt) ((new-halt-value string))
  (declare (connected main-window (update-halt string)))
  (setf (q+:text halt-value) (format nil "~a" new-halt-value)))


(define-signal (main-window refresh-register-display) (string))

(define-slot (main-window refresh-register-display) ((register-name string))
  (declare (connected main-window (refresh-register-display string)))
  ;; TODO Refactor
  (setf (q+:text r1-value) (format nil "~a" (slot-value (cpu main-window) 'r1)))
  (setf (q+:text r2-value) (format nil "~a" (slot-value (cpu main-window) 'r2)))
  (setf (q+:text r3-value) (format nil "~a" (slot-value (cpu main-window) 'r3)))
  (setf (q+:text r4-value) (format nil "~a" (slot-value (cpu main-window) 'r4)))
  (setf (q+:text r5-value) (format nil "~a" (slot-value (cpu main-window) 'r5)))
  (setf (q+:text r6-value) (format nil "~a" (slot-value (cpu main-window) 'r6)))
  (setf (q+:text r7-value) (format nil "~a" (slot-value (cpu main-window) 'r7)))
  (setf (q+:text r8-value) (format nil "~a" (slot-value (cpu main-window) 'r8))))

(define-signal (main-window print-char) (int))

(define-slot (main-window step-button) ()
  (declare (connected step-button (released)))
  (synacor:step! (cpu main-window)
                 ;; TODO factor this out
                 :callback (lambda ()
                             (signal! main-window (refresh-register-display string)
                                      "r1")
                             (signal! main-window (update-pc int) (pc cpu))
                             (signal! main-window (update-halt string)
                                      (if (halt cpu)
                                          "HALT"
                                          "-"))
                             (dump memory-dump (cpu main-window))
                             (let ((ch (synacor:read-out-bus cpu)))
                               (when ch
                                 (signal! main-window (print-char int) ch))))))

(define-slot (main-window run-button) ()
  (declare (connected run-button (released)))
  (let ((cpu (cpu main-window)))
    (synacor:run! cpu
                  ;; TODO factor this out
                  :callback (lambda ()
                              (signal! main-window (refresh-register-display string)
                                       "r1")
                              (signal! main-window (update-pc int) (pc cpu))
                             (signal! main-window (update-halt string)
                                      (if (halt cpu)
                                          "HALT"
                                          "-"))
                             (dump memory-dump (cpu main-window))
                              (let ((ch (synacor:read-out-bus cpu)))
                                (when ch
                                  (signal! main-window (print-char int) ch)))))))

(define-slot (main-window reset-button) ()
  (declare (connected reset-button (released)))
  (synacor:reset-cpu! (cpu main-window))
  (signal! main-window (update-pc int) (pc cpu))
  (signal! main-window (update-halt int)
           (if (halt cpu)
               "HALT"
               "-"))
  (signal! main-window (refresh-register-display string) "r1")
  (dump memory-dump (cpu main-window))
  (q+:show-message status-bar "CPU reset"))

(define-slot (main-window clear-output-button) ()
  (declare (connected clear-output-button (released)))
  (q+:clear output-text-area))

(define-signal (main-window program-loaded) (string))

(define-slot (main-window program-loaded) ((program-file string))
  (declare (connected main-window (program-loaded string)))
  (q+:show-message status-bar (format nil "~a loaded." program-file))
  (dump memory-dump (cpu main-window)))

(define-slot (main-window print-char) ((ch int))
  (declare (connected main-window (print-char int)))
  (q+:move-cursor output-text-area (q+:qtextcursor.end))
  (q+:insert-plain-text output-text-area (format nil "~c" (code-char ch)))
  (q+:move-cursor output-text-area (q+:qtextcursor.end)))

(define-slot (main-window program-loaded) ((program-file string))
  (declare (connected main-window (program-loaded string)))
  (q+:show-message status-bar (format nil "~a loaded." program-file)))

#|
[12:23 PM] phoe: with qtools, you must be wary which ABORT restart you use
[12:23 PM] Duuqnd: oh, I see
[12:23 PM] phoe: if you use the "restart thread" one or something then qt gets fucked up
[12:24 PM] Duuqnd: SDL has problems like that too
[12:24 PM] phoe: because its qapplication stays bound to a thread that is already dead
[12:24 PM] phoe: and you cannot really restart it
|#
(defun synacor-gui (cpu)
  (declare (ignorable cpu))
  (with-main-window
      (window (make-instance 'main-window :cpu cpu))))
