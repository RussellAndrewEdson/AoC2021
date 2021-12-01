;;; Code for Advent of Code 2021, Day 1.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 01/12/2021

;;; Problem 1
;; We are given a list of inputs to be read in:
(require :uiop)
(defparameter depth (mapcar #'parse-integer
			    (uiop:read-file-lines "day1_problem1.txt")))

;; First, we can set up a function to compute the differences
;; between consecutive elements in a given list (e.g. similar to
;; R's diff function).
(defun diff (x)
  "Return the differences between consecutive elements in X."
  (mapcar #'- (cdr x) (butlast x)))

;; Then we simply count the number of depth measurements that
;; increased from the previous measurement, i.e. the positive
;; entries in (diff depth). 
(length (remove-if-not (lambda (num) (> num 0)) (diff depth)))
;;=> 1696


;;; Problem 2
;; Now we consider three-measurement moving windows and compare the
;; sums similarly. So we sum up sets of three consecutive numbers and
;; then simply use the same (diff ..) function we coded earlier.
(defparameter 3-window
  (mapcar (lambda (window) (list (nth 0 window)
				 (nth 1 window)
				 (nth 2 window)))
	  (remove-if-not
	   (lambda (list) (>= (length list) 3))
	   (loop for moving-window on depth collect moving-window))))

(let ((window-sums (mapcar (lambda (window) (reduce #'+ window)) 3-window)))
  (length (remove-if-not
	   (lambda (num) (> num 0))
	   (diff window-sums))))
;;=> 1737
