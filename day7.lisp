;;; Code for Advent of Code 2021, Day 7.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 07/12/2021

;;; Problem 1
;; Here we are given a list of numbers that represent horizontal
;; positions for the 'crab submarines':
(ql:quickload '(:uiop :str :alexandria))
(defparameter crabs
  (mapcar #'parse-integer
	  (str:split "," (car (uiop:read-file-lines "day7_problem1.txt")))))

;; For part 1, we can simply exhaustive search over all of the possible
;; alignments and test the fuel requirements, then pick the minimum
;; of these.
(defun fuel (x)
  "Return the fuel requirements for aligning at horizontal position X."
  (reduce #'+ (mapcar (lambda (pos) (abs (- x pos))) crabs)))

(fuel 2)
;;=> 472640

;; Then the minimum-fuel position (and fuel count) is:
(alexandria:extremum
 (loop for x from (apply #'min crabs) upto (apply #'max crabs)
       collect (list x (fuel x)))
 #'<
 :key #'cadr)
;;=> (362 342534)


;;; Problem 2
;; Now we are given that the fuel consumption is slightly different:
;; instead of a constant fuel uptake, the fuel decreases the further
;; we move (i.e. so instead of the distance to x being pos - x, it is
;; triangle(pos - x), where triangle(n) is a function that returns the
;; nth triangle number.
(defun triangle (n)
  "Return the Nth triangle number."
  (loop for i from 1 upto n summing i))

;; So we redefine our fuel function:
(defun fuel (x)
  "Return the fuel requirements for aligning at horizontal position X."
  (reduce #'+ (mapcar (lambda (pos) (triangle (abs (- x pos)))) crabs)))

;; Now the minimum-fuel position (and fuel count) is:
(alexandria:extremum
 (loop for x from (apply #'min crabs) upto (apply #'max crabs)
       collect (list x (fuel x)))
 #'<
 :key #'cadr)
;;=> (474 94004208)
