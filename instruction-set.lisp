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
  (set-address! a (get-value b cpu) cpu))

;; EQ a b c -- 4
;;   set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
(instr :eq 4 (a b c)
  (set-address! a
                (if (eql (get-value b cpu)
                         (get-value c cpu))
                    1
                    0) *cpu*))

;; JMP A -- 6
;;   jump to <a>
(instr :jmp 6 (a)
  (setf (pc cpu) a))

;; JT A B -- 7
;;   if <a> is nonzero, jump to <b>
(instr :jt 7 (a b)
  (if (not (zerop (get-value a cpu)))
      (setf (pc cpu) (get-value b cpu))))

;; JF A B -- 8
;;   if <a> is zero, jump to <b>
(instr :jf 8 (a b)
  (if (zerop (get-value a cpu))
      (setf (pc cpu) (get-value b cpu))))

;; ADD A B C
;;   assign into <a> the sum of <b> and <c> (modulo 32768)
(instr :add 9 (a b c)
  (set-address! a (mod (+ (get-value b cpu)
                          (get-value c cpu))
                       32768)
                cpu))

;; OUT A -- 19
;;   write the character represented by ascii code <a> to the terminal
(instr :out 19 (a)
  (princ (code-char (get-value a cpu))))
