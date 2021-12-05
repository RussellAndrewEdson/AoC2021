;;; Code for Advent of Code 2021, Day 5.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 05/12/2021

;;; Problem 1
;; In this problem we are given a list of line segments
;; in the form "x1,y1 -> x2,y2" that we need to parse (e.g.
;; into a list of tuple lists, for simplicity):
(ql:quickload '(:uiop :str :alexandria))
(defparameter lines
  (mapcar (lambda (pair)
	    (mapcar (lambda (coordinates)
		      (mapcar #'parse-integer (str:split "," coordinates)))
		    pair))
	  (mapcar (lambda (line) (str:split " -> " line))
		  (uiop:read-file-lines "day5_problem1.txt"))))

;; For now, we restrict our attention to only those line segments
;; that are horizontal or vertical.
(defun vertical? (line)
  "True if the given LINE is vertical, false otherwise."
  (destructuring-bind (x1 y1 x2 y2) (alexandria:flatten line)
    (= x1 x2)))

(defun horizontal? (line)
  "True if the given LINE is horizontal, false otherwise."
  (destructuring-bind (x1 y1 x2 y2) (alexandria:flatten line)
    (= y1 y2)))

(defparameter hv-lines
  (remove-if-not (lambda (line)
		   (or (vertical? line) (horizontal? line)))
		 lines))

;; We want to 'draw' the diagram with these lines and count the
;; overlaps. To do this, we'll need a function that fills in
;; the points between (x1,y1) and (x2,y2) for these horizontal and
;; vertical lines, which is straightforward:
(defun fill-in-line (line)
  "Return all the points between (X1,Y1) and (X2,Y2)."
  (destructuring-bind (x1 y1 x2 y2) (alexandria:flatten line)
    (if (vertical? line)
	(let ((ymin (min y1 y2))
	      (ymax (max y1 y2)))
	  (loop for j from ymin upto ymax collect (list x1 j)))
	;; Otherwise (for now), we have a horizontal line
	(let ((xmin (min x1 x2))
	      (xmax (max x1 x2)))
	  (loop for i from xmin to xmax collect (list i y1))))))

;; Now we want to determine the bounds of our diagram so we can set
;; up a blank 'canvas' with the right dimensions.
(defparameter xs
  (loop for line in lines
	append
	(destructuring-bind (x1 y1 x2 y2) (alexandria:flatten line)
	  (list x1 x2))))
(apply #'min xs)
;;=> 10
(apply #'max xs)
;;=> 989

(defparameter ys
  (loop for line in lines
	append
	(destructuring-bind (x1 y1 x2 y2) (alexandria:flatten line)
	  (list y1 y2))))
(apply #'min ys)
;;=> 10
(apply #'max ys)
;;=> 989

;; (Safely) Assuming that the top-corner is (0,0), then a 1000x1000
;; integer array should suit for our canvas. If we set the default
;; value to 0, then we simply increment to draw a line, and it will
;; automatically take care of the overlaps for us.
(defparameter diagram (make-array '(1000 1000) :initial-element 0))
(loop for line in hv-lines do
  (let ((points (fill-in-line line)))
    (loop for point in points do
      (incf (aref diagram (car point) (cadr point))))))

;; Now we simply count the number of elements in diagram that are
;; greater than or equal to 2.
(loop for i from 0 below 1000
      summing
      (loop for j from 0 below 1000
	    when (>= (aref diagram i j) 2)
	      count j))
;;=> 4993


;;; Problem 2
;; Now we consider diagonal lines too (i.e. 45-degree lines), which
;; make up the rest of the lines given in the input. Fortunately, all
;; we need to do here is redefine the fill-in-line function above and
;; the rest of the code will work exactly the way we want.
(defun fill-in-line (line)
  "Return all the points between (X1,Y1) and (X2,Y2)."
  (destructuring-bind (x1 y1 x2 y2) (alexandria:flatten line)
    (if (vertical? line)
	(let ((ymin (min y1 y2))
	      (ymax (max y1 y2)))
	  (loop for j from ymin upto ymax collect (list x1 j)))
	(if (horizontal? line)
	    (let ((xmin (min x1 x2))
		  (xmax (max x1 x2)))
	      (loop for i from xmin to xmax collect (list i y1)))
	    ;; Otherwise, assume we have a diagonal line.
	    (let* ((min-x-point (alexandria:extremum line #'< :key #'car))
		   (max-x-point (alexandria:extremum line #'> :key #'car))
		   (y-inc (if (< (cadr min-x-point) (cadr max-x-point)) 1 -1)))
	      (loop for i from (car min-x-point) upto (car max-x-point)
		    and j = (cadr min-x-point) then (+ y-inc j)
		    collect (list i j)))))))

;; So we redraw the diagram, this time taking the diagonal lines into
;; account for the overlapping.
(defparameter diagram (make-array '(1000 1000) :initial-element 0))
(loop for line in lines do
  (let ((points (fill-in-line line)))
    (loop for point in points do
      (incf (aref diagram (car point) (cadr point))))))

;; Now we simply count the number of elements in diagram that are
;; greater than or equal to 2.
(loop for i from 0 below 1000
      summing
      (loop for j from 0 below 1000
	    when (>= (aref diagram i j) 2)
	      count j))
;;=> 21101
