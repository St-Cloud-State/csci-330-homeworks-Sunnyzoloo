;;; Recursive Descent Parser for the given grammar
;;; Grammar:
;;; I → iES | iESeS
;;; E → x | EoG
;;; G → x | y | z | w | v | o
;;; S → s | dLb
;;; L → s | sL

;;; Left-recursion eliminated and corrected:
;;; I → iES | iESeS
;;; E → xE'
;;; E' → oGE' | ε
;;; G → x | y | z | w | v | o
;;; S → s | dLb
;;; L → s | sL

;;; Forward declarations to avoid style warnings
(declaim (ftype function parse-i parse-e parse-e-prime parse-g parse-s parse-l parse))

;;; Position tracking and error handling
(defvar *input* nil)
(defvar *position* 0)
(defvar *errors* nil)

(defun current-char ()
  "Get the current character in the input string"
  (if (< *position* (length *input*))
      (char *input* *position*)
      nil))

(defun advance ()
  "Move to the next character in the input"
  (incf *position*))

(defun match (expected)
  "Match the current character against expected"
  (let ((c (current-char)))
    (if (and c (char= c expected))
        (progn (advance) t)
        nil)))

(defun report-error (message)
  "Report a parsing error"
  (push (format nil "Error at position ~a: ~a" *position* message) *errors*)
  nil)

;;; Parser functions for each non-terminal

(defun parse-i ()
  "Parse I production: I → iES | iESeS"
  (if (match #\i)
      (and (parse-e)
           (parse-s)
           (if (match #\e)  ; Check for optional eS part
               (parse-s)
               t))  ; If no 'e', that's fine too
      (report-error "Expected 'i'")))

(defun parse-e ()
  "Parse E production: E → xE'"
  (if (match #\x)
      (parse-e-prime)
      (report-error "Expected 'x'")))

(defun parse-e-prime ()
  "Parse E' production: E' → oGE' | ε"
  (if (match #\o)  ; Try to match 'o'
      (and (parse-g)
           (parse-e-prime))  ; If matched, must have G and then E'
      t))  ; ε case - always succeeds if no 'o'

(defun parse-g ()
  "Parse G production: G → x | y | z | w | v | o"
  (let ((c (current-char)))
    (cond ((null c) (report-error "Unexpected end of input"))
          ((member c '(#\x #\y #\z #\w #\v #\o))
           (advance)
           t)
          (t (report-error (format nil "Expected x, y, z, w, v, or o, got '~a'" c))))))

(defun parse-s ()
  "Parse S production: S → s | dLb"
  (let ((c (current-char)))
    (cond ((null c) (report-error "Unexpected end of input"))
          ((char= c #\s)
           (advance)
           t)  ;; Just consume 's' and succeed
          ((char= c #\d)
           (advance)
           (and (parse-l)
                (if (match #\b)
                    t
                    (report-error "Expected 'b'"))))
          (t (report-error (format nil "Expected 's' or 'd', got '~a'" c))))))

(defun parse-l ()
  "Parse L production: L → s | sL"
  (if (match #\s)
      (if (and (< *position* (length *input*))
               (char= (char *input* *position*) #\s))
          (parse-l)  ;; Try to parse another L if we see another 's'
          t)         ;; Otherwise just succeed with one 's'
      (report-error "Expected 's'")))

;;; Main parsing function
(defun parse (str)
  "Parse the input string according to the grammar"
  (setf *input* str)
  (setf *position* 0)
  (setf *errors* nil)
  (let ((result (parse-i)))
    (if (and result (>= *position* (length *input*)))
        (values t "Parsing successful!" nil)
        (values nil 
                (format nil "Parsing failed at position ~a" *position*)
                *errors*))))

;;; Test function
(defun test-parser ()
  "Test the parser with valid and invalid strings"
  (let ((valid-strings '("ixs" "ixoys" "ixoyovowdssbes" "idsbs" "ixoes" "ixoyes" "ixozes"))
        (invalid-strings '("xs" "ixoyt" "idsbx" "ixoyovowdssbef" "ixs$" "ixoyg" "iwoys")))
    
    (format t "~%Testing valid strings:~%")
    (dolist (str valid-strings)
      (multiple-value-bind (valid message errors) (parse str)
        (declare (ignore message))  ;; Properly ignore unused variable
        (format t "~a: ~a~%" str (if valid "VALID" "INVALID"))
        (unless valid
          (format t "  Errors: ~a~%" errors))))
    
    (format t "~%Testing invalid strings:~%")
    (dolist (str invalid-strings)
      (multiple-value-bind (valid message errors) (parse str)
        (declare (ignore message))  ;; Properly ignore unused variable
        (format t "~a: ~a~%" str (if valid "VALID" "INVALID"))
        (unless valid
          (format t "  Errors: ~a~%" errors))))))

;;; Enhanced error reporting
(defun parse-with-errors (str)
  "Parse with detailed error reporting"
  (setf *input* str)
  (setf *position* 0)
  (setf *errors* nil)
  (let ((result (parse-i)))
    (if (and result (>= *position* (length *input*)))
        (format t "Parsing successful!~%")
        (progn
          (format t "Parsing failed at position ~a~%" *position*)
          (format t "Unexpected symbol '~a' at position ~a~%" 
                  (if (< *position* (length *input*))
                      (char *input* *position*)
                      "END")
                  *position*)
          (when *errors*
            (format t "Errors:~%")
            (dolist (err *errors*)
              (format t "  ~a~%" err)))))))

;;; Generate test strings that match the grammar
(defun generate-valid-string ()
  "Generate a valid string according to the grammar"
  (let ((result "i"))
    (setf result (concatenate 'string result "x"))
    ;; Maybe add some oG sequences
    (dotimes (i (random 3))
      (setf result (concatenate 'string result "o"))
      (setf result (concatenate 'string result 
                               (string (nth (random 6) '(#\x #\y #\z #\w #\v #\o))))))
    ;; Add S
    (if (zerop (random 2))
        (setf result (concatenate 'string result "s"))
        (let ((l-string "s"))
          ;; Generate some random sL sequence
          (dotimes (i (random 3))
            (setf l-string (concatenate 'string l-string "s")))
          (setf result (concatenate 'string result "d" l-string "b"))))
    ;; Maybe add eS
    (when (zerop (random 2))
      (setf result (concatenate 'string result "e"))
      (if (zerop (random 2))
          (setf result (concatenate 'string result "s"))
          (let ((l-string "s"))
            ;; Generate some random sL sequence
            (dotimes (i (random 3))
              (setf l-string (concatenate 'string l-string "s")))
            (setf result (concatenate 'string result "d" l-string "b")))))
    result))

;;; Generate invalid strings by perturbing valid ones
(defun generate-invalid-string (valid-string)
  "Generate an invalid string by perturbing a valid one"
  (let* ((position (random (length valid-string)))
         (new-char (code-char (+ 97 (random 26)))))
    ;; Ensure the new character breaks the grammar
    (loop while (member new-char '(#\i #\x #\o #\y #\z #\w #\v #\s #\d #\b #\e))
          do (setf new-char (code-char (+ 97 (random 26)))))
    (concatenate 'string 
                 (subseq valid-string 0 position)
                 (string new-char)
                 (subseq valid-string (1+ position)))))

;;; Generate test strings
(defun generate-test-strings ()
  "Generate 7 valid and 7 invalid test strings"
  (let ((valid-strings nil)
        (invalid-strings nil))
    ;; Generate 7 valid strings
    (dotimes (i 7)
      (push (generate-valid-string) valid-strings))
    ;; Generate 7 invalid strings
    (dolist (str valid-strings)
      (push (generate-invalid-string str) invalid-strings))
    (format t "Generated valid strings:~%")
    (dolist (str valid-strings)
      (format t "~a~%" str))
    (format t "~%Generated invalid strings:~%")
    (dolist (str invalid-strings)
      (format t "~a~%" str))))

;;; Load and run tests
(defun load-and-test ()
  (test-parser)
  (format t "~%Testing with error reporting:~%")
  (parse-with-errors "ixoyg"))

;;; Run test on all examples
(defun run-all-examples ()
  (format t "~%Testing all examples:~%")
  (dolist (str '("ixs" "ixoys" "ixoyovowdssbes" "idsbs" "ixoes" "ixoyes" "ixozes"))
    (format t "~a: " str)
    (multiple-value-bind (valid message errors) (parse str)
      (declare (ignore message))  ;; Properly ignore unused variable
      (format t "~a~%" (if valid "VALID" "INVALID"))
      (when errors 
        (format t "  Errors: ~a~%" errors)))))

;;; Main function to run everything
(defun main ()
  (format t "~%===== Testing Parser =====~%")
  (test-parser)
  (format t "~%===== Generating Test Strings =====~%")
  (generate-test-strings)
  (format t "~%===== Running All Examples =====~%")
  (run-all-examples))