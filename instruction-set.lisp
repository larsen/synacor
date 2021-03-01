(in-package #:synacor)

;; HALT -- 0
;;   stop execution and terminate the program
(instr :halt 0 ()
    (setf (halt cpu) t))

;; NOOP -- 21
;;   no operation
(instr :noop 21 ())

;; SET A B -- 1
;;   set register <a> to the value of <b>
(instr :set 1 (a b)
  (set-address! a b cpu))

;; JMP A -- 6
;;   jump to <a>
(instr :jmp 6 (a)
  (setf (pc cpu) a))

;; JT A B -- 7
;;   if <a> is nonzero, jump to <b>
(instr :jt 7 (a b)
  (if (not (zerop a))
      (setf (pc cpu) (get-address b))))

;; JF A B -- 8
;;   if <a> is zero, jump to <b>
(instr :jf 8 (a b)
  (if (zerop a)
      (setf (pc cpu) (get-address b))))

;; OUT A -- 19
;;   write the character represented by ascii code <a> to the terminal
(instr :out 19 (a)
  (princ (code-char (get-address a cpu))))
