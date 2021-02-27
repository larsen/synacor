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
  (let* ((opcode (aref (mem cpu) (pc cpu)))
         (a nil)
         (b nil))
    (ecase opcode
      (0 ; halt
       (setf (halt cpu) t))
      (1 ; set A B
       (incpc! cpu)
       (setf a (aref (mem cpu) (pc cpu)))
       (incpc! cpu)
       (setf b (aref (mem cpu) (pc cpu)))
       (set-address! a b cpu)
       (incpc! cpu))
      (6 ; jmp A
       (incpc! cpu)
       (setf a (aref (mem cpu) (pc cpu)))
       (setf (pc cpu) a))
      (7 ; jt A B: if A is nonzero, jump to B
       (incpc! cpu)
       (setf a (aref (mem cpu) (pc cpu)))
       (incpc! cpu)
       (setf b (aref (mem cpu) (pc cpu)))
       (if (not (zerop a))
           (setf (pc cpu)
                 (get-address b cpu))
           (incpc! cpu)))
      (8 ; jf A B: if A is zero, jump to B
       (incpc! cpu)
       (setf a (aref (mem cpu) (pc cpu)))
       (incpc! cpu)
       (setf b (aref (mem cpu) (pc cpu)))
       (if (zerop a)
           (setf (pc cpu)
                 (get-address b cpu))
           (incpc! cpu)))
      (19 ; write char
       (incpc! cpu)
       (princ (code-char (get-address (pc cpu) cpu)))
       (incpc! cpu))
      (21 ; noop
       (incpc! cpu)))))

(defmethod run! ((cpu cpu))
  (loop while (not (halt cpu))
        do (exec-instruction! cpu)))

