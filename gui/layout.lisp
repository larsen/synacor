(in-package #:synacor-gui)
(in-readtable :qtools)

(define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
  (setf (q+:window-title main-window) "Synacor Debugger")

  (let ((inner-display-section (q+:make-qhboxlayout))
        (inner-buttons-section (q+:make-qhboxlayout))
        (status-section (q+:make-qvboxlayout))
        (pc-section (q+:make-qhboxlayout))
        (registers-section-1 (q+:make-qhboxlayout))
        (registers-section-2 (q+:make-qhboxlayout)))
    (q+:add-widget inner-display-section output-text-area)

    (q+:add-widget pc-section (q+:make-qlabel "PC" main-window))
    (q+:add-widget pc-section pc-value)

    (q+:add-widget pc-section (q+:make-qlabel "Halt" main-window))
    (q+:add-widget pc-section halt-value)

    (q+:add-widget registers-section-1 (q+:make-qlabel "R1" main-window))
    (q+:add-widget registers-section-1 r1-value)
    (q+:add-widget registers-section-1 (q+:make-qlabel "R2" main-window))
    (q+:add-widget registers-section-1 r2-value)
    (q+:add-widget registers-section-1 (q+:make-qlabel "R3" main-window))
    (q+:add-widget registers-section-1 r3-value)
    (q+:add-widget registers-section-1 (q+:make-qlabel "R4" main-window))
    (q+:add-widget registers-section-1 r4-value)

    (q+:add-widget registers-section-2 (q+:make-qlabel "R5" main-window))
    (q+:add-widget registers-section-2 r5-value)
    (q+:add-widget registers-section-2 (q+:make-qlabel "R6" main-window))
    (q+:add-widget registers-section-2 r6-value)
    (q+:add-widget registers-section-2 (q+:make-qlabel "R7" main-window))
    (q+:add-widget registers-section-2 r7-value)
    (q+:add-widget registers-section-2 (q+:make-qlabel "R8" main-window))
    (q+:add-widget registers-section-2 r8-value)

    (q+:add-layout status-section pc-section)
    (q+:add-layout status-section registers-section-1)
    (q+:add-layout status-section registers-section-2)
    (q+:add-widget status-section memory-dump)
    (q+:add-layout inner-display-section status-section)

    (q+:add-widget inner-buttons-section step-button)
    (q+:add-widget inner-buttons-section run-button)
    (q+:add-widget inner-buttons-section reset-button)
    (q+:add-widget inner-buttons-section clear-output-button)

    (q+:add-widget inner-buttons-section status-bar)

    ;; packing into the main section
    (q+:add-layout layout inner-display-section)
    (q+:add-layout layout inner-buttons-section))
  (let ((widget (q+:make-qwidget main-window)))
    (setf (q+:layout widget) layout)
    (setf (q+:central-widget main-window) widget))
  (q+:resize main-window 500 1000))
