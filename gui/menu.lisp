(in-package #:synacor-gui)
(in-readtable :qtools)

(define-menu (main-window File)
  (:item ("Open" (ctrl o))
         (let ((program-file (q+:qfiledialog-get-open-file-name
                              main-window "Select Program File"
                              (format nil "~a" (asdf:system-source-directory 'synacor)))))
           (when (uiop:file-exists-p program-file)
             (synacor:load! (synacor:read-bin-file program-file) (cpu main-window))
             (signal! main-window (program-loaded string) program-file))))
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About" (q+:qmessagebox-information
                  main-window "About"
                  "Synacor Challenge CPU Debugger")))
