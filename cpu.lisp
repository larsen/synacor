(in-package #:synacor)

(defclass cpu ()
  (
   ;; Program counter
   (pc :initform 0 :accessor pc)
   ;; Halt state
   (halt :initform nil :accessor halt)
   ;; Registers
   (r1 :initform 0 :accessor r1)
   (r2 :initform 0 :accessor r2)
   (r3 :initform 0 :accessor r3)
   (r4 :initform 0 :accessor r4)
   (r5 :initform 0 :accessor r5)
   (r6 :initform 0 :accessor r6)
   (r7 :initform 0 :accessor r7)
   (r8 :initform 0 :accessor r8)
   ;; Memory
   (mem :initform (make-array 32768)
        :accessor mem)
   (stack :initform '()
          :accessor stack)
   ;; Bus
   (in :accessor bus-in)
   (out :accessor bus-out)
   ;; Inspection
   (breakpoints :initform '()
                :accessor breakpoints)))

(defgeneric incpc! (cpu))
(defmethod incpc! ((cpu cpu))
  (incf (pc cpu)))

(defgeneric print-char (ch cpu))
(defmethod print-char (ch (cpu cpu))
  (setf (bus-out cpu) ch))

(defgeneric read-out-bus (cpu))
(defmethod read-out-bus ((cpu cpu))
  (handler-case
      (let ((ch (bus-out cpu)))
        (setf (bus-out cpu) nil)
        ch)
    (unbound-slot ())))

(defgeneric set-address! (address value cpu))
(defmethod set-address! (address value (cpu cpu))
  "Sets the object (memory location or register) denoted by ADDRESS to VALUE."
  ;; - numbers 0..32767 mean a literal value
  ;; - numbers 32768..32775 instead mean registers 1..8
  (if (and (>= address 0)
           (<= address 32767))
      (setf (aref (mem cpu) address) value)
      (setf (slot-value cpu
                        (read-from-string (format nil "R~d" (+ 1 (- address 32768)))))
            value)))

(defgeneric get-address (address cpu))
(defmethod get-address (address (cpu cpu))
  (if (and (>= address 0)
           (<= address 32767))
      (aref (mem cpu) address)
      (slot-value cpu
                  (read-from-string (format nil "R~d" (+ 1 (- address 32768)))))))

(defgeneric get-value (value cpu))
(defmethod get-value (value (cpu cpu))
  (if (and (>= value 0)
           (<= value 32767))
      value
      (slot-value cpu
                  (read-from-string (format nil "R~d" (+ 1 (- value 32768)))))))

(defgeneric print-object (cpu stream))
(defmethod print-object ((cpu cpu) stream)
  (format stream "PC: ~5,'0d Halt: ~a~%" (pc cpu) (halt cpu))
  (format stream "R1: ~5,'0d R2: ~5,'0d R3: ~5,'0d R4: ~5,'0d~%"
          (r1 cpu) (r2 cpu) (r3 cpu) (r4 cpu))
  (format stream "R5: ~5,'0d R6: ~5,'0d R7: ~5,'0d R8: ~5,'0d~%"
          (r5 cpu) (r6 cpu) (r7 cpu) (r8 cpu))
  (format stream "Stack: ~a~%" (stack cpu)))

(defgeneric load! (program cpu))
(defmethod load! (program (cpu cpu))
  "Loads a PROGRAM (list of words) into the CPU's memory"
  (loop for idx from 0
        for word in program
        do (setf (aref (mem cpu) idx) word)))

(defgeneric registers-state (cpu))
(defmethod registers-state ((cpu cpu))
  (loop for reg in '(r1 r2 r3 r4 r5 r6 r7 r8)
        collect (slot-value cpu reg)))

(defgeneric reset-cpu! (cpu))
(defmethod reset-cpu! ((cpu cpu))
  "Reset CPU internal state"
  (setf (pc cpu) 0)
  (setf (halt cpu) nil)
  (setf (stack cpu) nil)
  (loop for reg in '(r1 r2 r3 r4 r5 r6 r7 r8)
        do (setf (slot-value cpu reg) 0))
  cpu)

(defgeneric set-breakpoint! (instruction-pointer cpu))
(defmethod set-breakpoint! (instruction-pointer (cpu cpu))
  (push instruction-pointer (breakpoints cpu)))

(defgeneric exec-instruction! (cpu))
(defmethod exec-instruction! ((cpu cpu))
  (let* ((opcode (aref (mem cpu) (pc cpu))))
    (exec! opcode cpu)))

(defgeneric run! (cpu &key))
(defmethod run! ((cpu cpu) &key (with-instruction-log nil) (callback nil provided-callback-p))
  (let ((instruction-log '()))
    (loop while (not (halt cpu))
          if (member (pc cpu) (breakpoints cpu))
            do (error "Execution interrupted for breakpoint!")
          if with-instruction-log
            do (push (list (pc cpu)
                           (disassemble-instruction-at-point cpu :instruction-pointer (pc cpu))
                           (registers-state cpu))
                     instruction-log)
          do (progn (exec-instruction! cpu)
                    (when provided-callback-p
                      (funcall callback))))
    (if with-instruction-log
        instruction-log)))

(defgeneric step! (cpu &key))
(defmethod step! ((cpu cpu) &key (instruction-pointer nil provided-instruction-pointer-p)
                              (callback nil provided-callback-p))
  (when provided-instruction-pointer-p
    (setf (pc cpu) instruction-pointer))
  (exec-instruction! cpu)
  (when provided-callback-p
    (funcall callback))
  (print cpu))

(defgeneric exec! (opcode cpu))
(defmethod exec! (opcode cpu)
  (declare (ignore cpu))
  (error "You should implement exec! for opcode ~a." opcode))

(defgeneric disassemble-instruction (opcode cpu &key))
(defmethod disassemble-instruction (opcode cpu &key instruction-pointer)
  (declare (ignore instruction-pointer))
  (error "You should implement disassemble-instruction for opcode ~a." opcode))

(defun address-representation (address)
  (if (and (>= address 0)
           (<= address 32767))
      address
      (format nil "R~d" (+ 1 (- address 32768)))))

(defun disassemble-format-string (instruction-size)
  (case instruction-size
    (1 "~6,'0d: ~6,'0d                      ; ~a")
    (2 "~6,'0d: ~6,'0d ~6,'0d               ; ~a ~a")
    (3 "~6,'0d: ~6,'0d ~6,'0d ~6,'0d        ; ~a ~a ~a")
    (4 "~6,'0d: ~6,'0d ~6,'0d ~6,'0d ~6,'0d ; ~a ~a ~a ~a")))

(defun disassemble-format-args (opcode instruction-name instruction-size cpu pc)
  (case instruction-size
    (1 (list pc (aref (mem cpu) pc) instruction-name))
    (2 (list pc (aref (mem cpu) pc)
             (aref (mem cpu) (+ 1 pc))
             instruction-name
             (case opcode
               (19 (code-char (aref (mem cpu) (+ 1 pc))))
               (otherwise (address-representation (aref (mem cpu) (+ 1 pc)))))))
    (3 (list pc (aref (mem cpu) pc)
             (aref (mem cpu) (+ 1 pc))
             (aref (mem cpu) (+ 2 pc))
             instruction-name
             (address-representation (aref (mem cpu) (+ 1 pc)))
             (address-representation (aref (mem cpu) (+ 2 pc)))))
    (4 (list pc (aref (mem cpu) pc)
             (aref (mem cpu) (+ 1 pc))
             (aref (mem cpu) (+ 2 pc))
             (aref (mem cpu) (+ 3 pc))
             instruction-name
             (address-representation (aref (mem cpu) (+ 1 pc)))
             (address-representation (aref (mem cpu) (+ 2 pc)))
             (address-representation (aref (mem cpu) (+ 3 pc)))))))

(defmacro instr (instruction-name opcode params &body body)
  (declare (ignorable params))
  `(progn
     (defmethod exec! ((opcode (eql ,opcode)) (cpu cpu))
       (incpc! cpu) ;; Skipping the opcode word
       (let (,@(loop for p in params
                     collect `(,p nil)))
         ,@(when params
             (loop for p in params
                   collect `(setf ,p (aref (mem cpu) (pc cpu)))
                   collect '(incpc! cpu)))
         ,@body))
     (defmethod disassemble-instruction ((opcode (eql ,opcode)) (cpu cpu)
                                         &key instruction-pointer)
       (let* ((pc (or instruction-pointer (pc cpu)))
              (instruction-size (+ 1 (length ',params)))
              (format-string (disassemble-format-string instruction-size))
              (format-args (disassemble-format-args
                            opcode ,instruction-name instruction-size cpu
                            instruction-pointer)))
         (values (apply #'format nil format-string format-args)
                 (+ instruction-size pc))))))

(defgeneric disassemble-instruction-at-point (cpu &key))
(defmethod disassemble-instruction-at-point ((cpu cpu) &key (steps 1) instruction-pointer)
  "Return a disassembled version of the instruction(s) the current PC is pointing to."
  (declare (ignorable instruction-pointer))
  (loop with loop-pc = (or instruction-pointer (pc cpu))
        for s from 1 to steps
        for opcode = (aref (mem cpu) loop-pc)
        collect (destructuring-bind (instruction-representation new-pc)
                    (multiple-value-list (disassemble-instruction
                                          opcode cpu
                                          :instruction-pointer loop-pc))
                  (setf loop-pc new-pc)
                  instruction-representation)))
