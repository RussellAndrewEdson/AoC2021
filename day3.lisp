;;; Code for Advent of Code 2021, Day 3.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 03/12/2021

;;; Problem 1
;; Now we are given a list of binary numbers to parse.
(require :uiop)

(defparameter diagnostic-report
  (mapcar (lambda (string)
	    (mapcar #'digit-char-p
		    (coerce string 'list)))
	  (uiop:read-file-lines "day3_problem1.txt")))

;; For part 1, we are given that the gamma rate is a binary number
;; where the nth digit is the most common bit in the nth position
;; of each of the binary numbers from the report. (In the case of a
;; tie, we'll return a :TIE keyword.)
(defun most-common-bits (report)
  "The gamma rate (i.e. most common bits in each position) for REPORT."
  (let ((half (/ (length report) 2)))
    (mapcar (lambda (sum)
	      (cond ((> sum half) 1)
		    ((< sum half) 0)
		    (t :tie)))
	    (apply #'mapcar #'+ report))))

(defparameter gamma (coerce (most-common-bits diagnostic-report) 'bit-vector))
gamma
;;=> #*010100111001

;; Similarly, the epsilon rate is the binary number constructed from
;; the least common bits (i.e. so it is exactly the complement of gamma).
(defparameter epsilon (bit-not gamma))
epsilon
;;=> #*101011000110

;; So we simply convert these binary numbers to decimal and then multiply
;; them together to get the power consumption for our answer.
(defun bit-vector->integer (bit-vector)
  "Convert the given BIT-VECTOR to its base-10 integer representation."
  (let ((reordered-bits (reverse bit-vector)))
    (loop for index from 0 below (length reordered-bits)
	  summing
	  (* (elt reordered-bits index) (expt 2 index)))))

(* (bit-vector->integer gamma) (bit-vector->integer epsilon))
;;=> 3687446


;;; Problem 2
;; Next we find the 'oxygen generator rating' by using a similar idea
;; of locating the most/least common bits, but at each step we retain
;; only those binary strings that contain that most/least common bit in
;; that position (breaking ties by choosing 1 instead). When we are left
;; with only one number, then this is the oxygen generator rating.
(defparameter oxygen-generator
  (let ((candidates diagnostic-report)
	(index 0))
    (loop while (and (> (length candidates) 1)
		     (< index (length (elt candidates 0))))
	  do
	     (let ((most-common-bit (elt (most-common-bits candidates) index)))
	       (setf most-common-bit
		     (if (eql most-common-bit :tie) 1 most-common-bit))
	       (setf candidates
		     (remove-if-not
		      (lambda (bits) (= (elt bits index) most-common-bit))
		      candidates))
	       (incf index)))
    (coerce (car candidates) 'bit-vector)))
oxygen-generator
;;=> #*011000111111

;; Similarly, the 'CO2 scrubber rating' is found by the same process,
;; but keeping only the least common bit strings and choosing 0 for the
;; tie-breaker. (Again, we exploit the fact that the least common bit
;; is simple the complement of the most common bit, which even means
;; that if we resolve the tie with 1 before taking the complement then
;; everything works.)
(defparameter co2-scrubber
  (let ((candidates diagnostic-report)
	(index 0))
    (loop while (and (> (length candidates) 1)
		     (< index (length (elt candidates 0))))
	  do
	     (let ((most-common-bit (elt (most-common-bits candidates) index)))
	       (setf most-common-bit
		     (if (eql most-common-bit :tie) 1 most-common-bit))
	       (let ((least-common-bit (if (= most-common-bit 1) 0 1)))
		 (setf candidates
		       (remove-if-not
			(lambda (bits) (= (elt bits index) least-common-bit))
			candidates))
		 (incf index))))
    (coerce (car candidates) 'bit-vector)))
co2-scrubber
;;=> #*101011000100

;; Finally, we multiply the numbers together to get the 'life
;; support rating' and our answer.
(* (bit-vector->integer oxygen-generator) (bit-vector->integer co2-scrubber))
;;=> 4406844
