(in-package #:synacor)

(defvar *challenge-bin*
  (asdf:system-relative-pathname 'synacor "bins/challenge.bin"))

(defun read-value (stream)
  "Read a 16-bit little-endian word from STREAM."
  (let ((lb (read-byte stream nil 'eof))
        (hb (read-byte stream nil 'eof)))
    (when (and (not (eql lb 'eof))
               (not (eql hb 'eof)))
      (+ (ash hb 8) lb))))

(defun read-bin-file (filename)
  "Read a binary file FILENAME following the Synacor specs.
Return a list of 16-bit words representing a program."
  (with-open-file (s filename
                     :direction :input
                     :element-type 'unsigned-byte)
    (loop for v = (read-value s)
          until (null v)
          collect v)))
