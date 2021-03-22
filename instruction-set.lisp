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

;; PUSH a -- 2
;;   push <a> onto the stack
(instr :push 2 (a)
  (push (get-value a cpu) (stack cpu)))

;; POP a -- 3
;;   remove the top element from the stack and write it into <a>; empty stack = error
(instr :pop 3 (a)
  (set-address! a (pop (stack cpu)) cpu))

;; EQ a b c -- 4
;;   set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
(instr :eq 4 (a b c)
  (set-address! a
                (if (eql (get-value b cpu)
                         (get-value c cpu))
                    1
                    0) cpu))

;; GT a b c -- 5
;;   set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
(instr :gt 5 (a b c)
  (set-address! a
                (if (> (get-value b cpu)
                       (get-value c cpu))
                    1
                    0) cpu))

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

;; ADD A B C -- 9
;;   assign into <a> the sum of <b> and <c> (modulo 32768)
(instr :add 9 (a b c)
  (set-address! a (mod (+ (get-value b cpu)
                          (get-value c cpu))
                       32768)
                cpu))

;; MULT a b c -- 10
;;   store into <a> the product of <b> and <c> (modulo 32768)
(instr :mult 10 (a b c)
  (set-address! a (mod (* (get-value b cpu)
                          (get-value c cpu))
                       32768)
                cpu))

;; MOD a b c -- 11
;;   store into <a> the remainder of <b> divided by <c>
(instr :mod 11 (a b c)
  (set-address! a (mod (get-value b cpu)
                       (get-value c cpu))
                cpu))

;; AND a b c -- 12
;;   stores into <a> the bitwise and of <b> and <c>
(instr :and 12 (a b c)
  (set-address! a (logand (get-value b cpu)
                          (get-value c cpu))
                cpu))

;; OR a b c -- 13
;;   stores into <a> the bitwise or of <b> and <c>
(instr :or 13 (a b c)
  (set-address! a (logior (get-value b cpu)
                          (get-value c cpu))
                cpu))

;; NOT a b -- 14
;;  stores 15-bit bitwise inverse of <b> in <a>
(instr :not 14 (a b)
  (loop with acc = 0
        for d across (format nil "~15,'0b" (get-value b cpu))
        do (setf acc (+ (* 2 acc)
                        (if (eql d #\1) 0 1)))
        finally (set-address! a acc cpu)))

;; RMEM a b -- 15
;;   read memory at address <b> and write it to <a>
(instr :rmem 15 (a b)
  (set-address! a (get-address b cpu)
                cpu))

;; WMEM a b -- 16
;;   write the value from <b> into memory at address <a>
(instr :wmem 16 (a b)
  (set-address! a (get-value b cpu)
                cpu))

;; CALL a -- 17
;;   write the address of the next instruction to the stack and jump to <a>
(instr :call 17 (a)
  ;; When we reach this point, the PC has
  ;; already been incremented twice.
  (push (pc cpu) (stack cpu))
  (setf (pc cpu) (get-value a cpu)))

;; RET -- 18
;;  remove the top element from the stack and jump to it; empty stack = halt
(instr :ret 18 ()
  (let ((jump-destination (pop (stack cpu))))
    (if jump-destination
        (setf (pc cpu) jump-destination)
        (setf (halt cpu) t))))

;; OUT A -- 19
;;   write the character represented by ascii code <a> to the terminal
(instr :out 19 (a)
  ;; (code-char (get-value a cpu))
  (print-char (get-value a cpu)
              cpu))
