;;; Code for Advent of Code 2021, Day 10.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 11/12/2021

;;; Problem 1
;; We are given an input of parentheses and brackets:
(ql:quickload '(:uiop :str))
(defparameter lines (uiop:read-file-lines "day10_problem1.txt"))

;; We are to scan these lines to check for correct syntax: each
;; opening bracket must match with its exact closed bracket. A line
;; is considered corrupted if this is not the case.
;;
;; So a straightforward (if inefficient) way to code such a syntax
;; checker is to have a stack data structure that we push
;; consecutive parens/brackets on to. The top of the stack tells us
;; which closing delimiter to expect: if we ever get a closing
;; character that was unexpected, then the line is corrupted.
;;
;; We're also interested in the first illegal character, so since
;; strings are truthy in Common Lisp we'll have our 'corrupted?'
;; function return the first illegal character too. (And later
;; we'll be interested in the remaining stack characters too,
;; so we'll return those as an auxiliary value.)
(defun left-delimiter? (delimiter)
  "True if the given DELIMITER is a left delimiter."
  (or (equalp delimiter "(")
      (equalp delimiter "[")
      (equalp delimiter "{")
      (equalp delimiter "<")))

(defun right-delimiter? (delimiter)
  "True if the given DELIMITER is a right delimiter."
  (or (equalp delimiter ")")
      (equalp delimiter "]")
      (equalp delimiter "}")
      (equalp delimiter ">")))

(defun matching-delimiter (delimiter)
  "Return the match for the given DELIMITER."
  (cond ((equalp delimiter "(") ")")
	((equalp delimiter "[") "]")
	((equalp delimiter "{") "}")
	((equalp delimiter "<") ">")
	((equalp delimiter ")") "(")
	((equalp delimiter "]") "[")
	((equalp delimiter "}") "{")
	((equalp delimiter ">") "<")
	(t nil)))

(defun corrupted? (line)
  "Returns the first illegal character if LINE is corrupted, nil if not."
  (let ((line (str:split "" line :omit-nulls t))
	(stack nil)
	(illegal-character nil))
    (loop named syntax-check while (> (length line) 0) do
      (let ((next-delimiter (pop line)))
	(if (left-delimiter? next-delimiter)
	    (push next-delimiter stack)
	    (if (equalp (matching-delimiter (car stack)) next-delimiter)
		(pop stack)
		(progn
		  (setf illegal-character next-delimiter)
		  (return-from syntax-check))))))
    (values illegal-character stack)))

(corrupted? "(([]{}(")
;;=> NIL
;;=> ("(" "(" "(")

(corrupted? "(([]{}(]")
;;=> "]"
;;=> ("(" "(" "(")

;; We also want to score the illegal-characters on the corrupted lines
;; as follows:
(defun score (character)
  "Return the score for the given (illegal) CHARACTER."
  (cond ((equalp character ")") 3)
	((equalp character "]") 57)
	((equalp character "}") 1197)
	((equalp character ">") 25137)
	(t nil)))

;; Finally, we simply map over all of the lines, find the
;; corrupted ones, and sum up the scores.
(defparameter illegal-characters
  (remove-if #'null (mapcar #'corrupted? lines)))
illegal-characters
;;=> ("]" "]" "]" ">" ")" ")" ")" "}" "}" "}" "}" "}" ")" "}" ")" ")"
;;=>  ">" "]" ">" "]" ">" "}" ">" ">" ">" ")" "]" "}" ">" ">" ")" ")"
;;=>  ">" "}" ">" ">" ">" "]" "}" ")" ">" "}" ">" ">" ">" ")" ")" "}"
;;=>  ")")

(reduce #'+ (mapcar #'score illegal-characters))
;;=> 442131


;;; Problem 2
;; Now we discard the corrupted lines and work with the incomplete
;; lines from the given input:
(defparameter incomplete-lines (remove-if #'corrupted? lines))

;; Now we want to autocomplete the lines. Since we're returning
;; the stack in our 'corrupted?' function above, then we simply
;; get the matches of all of those characters and return them in
;; the completion order that guarantees legal syntax.
(defun auto-complete (line)
  "Return the delimiting characters that auto-complete the given LINE."
  (let ((opening-delimiters (nth-value 1 (corrupted? line))))
    (format nil "~{~a~}"
	    (loop while (> (length opening-delimiters) 0)
		  collect (matching-delimiter (pop opening-delimiters))))))

(auto-complete "(([]{}{[]<<")
;;=> ">>}))"

;; Now we want to score each of the completed sections according to the
;; algorithm given in the problem, so we'll code a function to do
;; exactly that:
(defun score (completion)
  "Return the score for the given auto-COMPLETION string."
  (let ((delimiters (str:split "" completion :omit-nulls t))
	(score 0))
    (loop for delimiter in delimiters do
      (setf score (+ (* score 5)
		     (cond ((equalp delimiter ")") 1)
			   ((equalp delimiter "]") 2)
			   ((equalp delimiter "}") 3)
			   ((equalp delimiter ">") 4)
			   (t 0)))))
    score))

(score "}}]])})]")
;;=> 288957

;; Finally, we compute the scores, sort them in ascending order,
;; and then take the middle score for our answer.
(defun median (values)
  "Return the median (middle value) of the given VALUES."
  (let ((n (length values))
	(sorted (sort (copy-seq values) #'<)))
    (elt sorted (floor (/ n 2)))))

(median (mapcar (lambda (line) (score (auto-complete line)))
		incomplete-lines))
;;=> 3646451424
