(in-package #:synacor)

(defvar *cpu* nil)

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
        :accessor mem)))

(defmethod incpc! ((cpu cpu))
  (incf (pc cpu)))

(defmethod set-address! (address value (cpu cpu))
  "Sets the object (memory location or register) denoted by ADDRESS to VALUE."
  ;; - numbers 0..32767 mean a literal value
  ;; - numbers 32768..32775 instead mean registers 0..7
  (if (and (> address 0)
           (<= address 32767))
      (setf (aref (mem cpu) address) value)
      (setf (slot-value cpu
                        (read-from-string (format nil "R~d" (+ 1 (- 32768 address)))))
            value)))

(defmethod get-address (address (cpu cpu))
  (if (and (> address 0)
           (<= address 32767))
      (aref (mem cpu) address)
      (slot-value cpu
                  (read-from-string (format nil "R~d" (+ 1 (- 32768 address)))))))

(defmethod print-object ((cpu cpu) stream)
  (format stream "PC: ~5,'0d~%" (pc cpu))
  (format stream "R1: ~5,'0d R2: ~5,'0d R3: ~5,'0d R4: ~5,'0d~%"
          (r1 cpu) (r2 cpu) (r3 cpu) (r4 cpu))
  (format stream "R5: ~5,'0d R6: ~5,'0d R7: ~5,'0d R8: ~5,'0d~%"
          (r5 cpu) (r6 cpu) (r7 cpu) (r8 cpu))
  (loop for idx from (pc cpu) upto (+ 15 (pc cpu))
        do (format stream "~5,'0d: ~5,'0d~%" idx (aref (mem cpu) idx))))

(defmethod load! (program (cpu cpu))
  "Loads a PROGRAM (list of words) into the CPU's memory"
  (loop for idx from 0
        for word in program
        do (setf (aref (mem cpu) idx) word)))

(defmethod reset-cpu! ((cpu cpu))
  "Reset CPU internal state"
  (setf (pc cpu) 0)
  (loop for reg in '(r1 r2 r3 r4 r5 r6 r7 r8)
        do (setf (slot-value cpu reg) 0))
  cpu)

(defmethod exec-instruction! ((cpu cpu))
  (let* ((opcode (aref (mem cpu) (pc cpu))))
    (exec! opcode cpu)))

(defmethod run! ((cpu cpu))
  (loop while (not (halt cpu))
        do (exec-instruction! cpu)))

(defmethod step! ((cpu cpu))
  (exec-instruction! cpu)
  (print cpu))

(defgeneric exec! (opcode cpu))
(defgeneric disassemble-instruction (opcode cpu))

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
     (defmethod disassemble-instruction ((opcode (eql ,opcode)) (cpu cpu))
       (values (format nil "~5,'0d            : ~a~%" (pc cpu) ,instruction-name)
               (+ ,(+ 1 (length params)) (pc cpu))))))

(defgeneric disassemble-instruction-at-point (cpu &key))
(defmethod disassemble-instruction-at-point ((cpu cpu) &key (steps 1) instruction-pointer)
  "Print a disassembled version of the instruction(s) the current PC is pointing to.
Returns a new value of the PC after consuming the instruction."
  (declare (ignore instruction-pointer))
  (loop with loop-pc = (pc cpu)
        for s from 1 to steps
        for opcode = (aref (mem cpu) loop-pc)
        do (destructuring-bind (instruction-representation new-pc)
               (multiple-value-list (disassemble-instruction opcode cpu))
             (print instruction-representation)
             (setf loop-pc new-pc))))
