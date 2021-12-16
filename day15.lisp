;;; Code for Advent of Code 2021, Day 15.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 16/12/2021

;;; Problem 1
;; For our puzzle input we are given a 100x100 grid of numbers, each
;; number representing our 'risk' level, and we want to find our
;; way from the top-left to the bottom-right, along the path of
;; minimum risk.
(ql:quickload '(:uiop :str :alexandria))
(defparameter risk-map
  (let ((risks (mapcar (lambda (string)
			 (mapcar #'parse-integer
				 (str:split "" string :omit-nulls t)))
		       (uiop:read-file-lines "day15_problem1.txt"))))
    (make-array (list (length risks) (length (car risks)))
		:initial-contents risks)))

;; We can use Dijkstra's algorithm here, so first we'll code some
;; utility functions. First, we want a function that, given an (i,j)
;; index, returns all of the adjacent indices (as long as they're
;; within the risk-map matrix).
(defun adjacent-indices (i j)
  "Return a list of indices adjacent to (I, J) in the risk map."
  (destructuring-bind (ymax xmax) (array-dimensions risk-map)
    (let ((ymin 0)
	  (xmin 0)
	  (adjacent nil))
      (if (> i ymin) (push (list (1- i) j) adjacent))
      (if (< i (1- ymax)) (push (list (1+ i) j) adjacent))
      (if (> j xmin) (push (list i (1- j)) adjacent))
      (if (< j (1- xmax)) (push (list i (1+ j)) adjacent))
      adjacent)))

(adjacent-indices 2 3)
;;=> ((2 4) (2 2) (3 3) (1 3))

;; Now we'll need a minimum risk function too. Since Common Lisp
;; traditionally doesn't define a notion of infinity, we'll use -1
;; instead and have a test for that here in this function.
;; (We also define a predicate for comparing risks.)
(defun minimum-risk (a b)
  "Return the minimum risk out of A and B."
  (cond ((= a -1) b)
	((= b -1) a)
	(t (if (< a b) a b))))

(defun smaller-risk? (a b)
  "True if A has a smaller risk than B."
  (= a (minimum-risk a b)))

(minimum-risk 1234 -1)
;;=> 1234

;; Finally, we can code an implementation of Dijkstra's algorithm
;; to find the minimum risk path from the top-left (0,0) index to
;; any other index in the risk map (including (99,99), our bottom
;; corner of interest).
(defun dijkstra (source)
  "Return the minimum risks from SOURCE to any other point in the risk map."
  (destructuring-bind (ymax xmax) (array-dimensions risk-map)
    (let ((indices-to-check
	    (loop for i from 0 below ymax
		  append
		  (loop for j from 0 below xmax
			collect (list i j))))
	  (risk (make-array (list ymax xmax) :initial-element -1)))
      (setf (aref risk (car source) (cadr source)) 0)
      (loop until (null indices-to-check) do
	(let* ((risks (mapcar (lambda (ij) (aref risk (car ij) (cadr ij)))
			      indices-to-check))
	       (min-risk (alexandria:extremum risks #'smaller-risk?))
	       (min-index (elt indices-to-check (position min-risk risks))))
	  (setf indices-to-check
		(remove min-index indices-to-check :test #'equalp))
	  (loop for adjacent in (intersection
				 (adjacent-indices (car min-index)
						   (cadr min-index))
				 indices-to-check
				 :test #'equalp)
		do
		   (let ((alt-risk (+ (aref risk
					    (car min-index)
					    (cadr min-index))
				      (aref risk-map
					    (car adjacent)
					    (cadr adjacent)))))
		     (if (= alt-risk (minimum-risk alt-risk
						   (aref risk
							 (car adjacent)
							 (cadr adjacent))))
			 (setf (aref risk (car adjacent) (cadr adjacent))
			       alt-risk))))))
      risk)))

;; So for this first part, we simply compute the minimum risks and
;; return the one for the bottom-right corner (i.e. index (99, 99)).
(time (defparameter min-risks (dijkstra '(0 0))))
;;=> Evaluation took:
;;=>   3.412 seconds of real time
;;=>   3.406250 seconds of total run time (3.015625 user, 0.390625 system)
;;=>   [ Run times consist of 0.250 seconds GC time, and 3.157 seconds non-GC time. ]
;;=>   99.82% CPU
;;=>   8,845,510,642 processor cycles
;;=>   1,603,414,976 bytes consed
;;=>
;;=> MIN-RISKS

(aref min-risks 99 99)
;;=> 390


;;; Problem 2
;; For this second part, we now expand the risk-map to be five times
;; larger in every direction, where each new tile repeats to the right
;; and downward but all of the risks increase by one (but wrap-around
;; from 9 back to 1). That is, if (0,0) is our original tile, then
;; the tile at (i, j) adds i and j to every risk, wrapping back around
;; to 1 if it exceeds 9.
(defun wrap-around (n)
  "Returns the wrap-around value of N (roughly but-not-quite modulo 9)."
  (let ((remainder (rem n 9)))
    (if (zerop remainder) 9 remainder)))

(wrap-around 9)
;;=> 9

(wrap-around 10)
;;=> 1

(wrap-around 11)
;;=> 2

;; So now we can redefine risk-map to contain this larger map:
(defparameter risk-map
  (let* ((dimensions (array-dimensions risk-map))
	 (new-map (make-array (list (* 5 (car dimensions))
				    (* 5 (cadr dimensions)))
			      :initial-element -1)))
    (loop for tile-i from 0 below 5 do
      (loop for tile-j from 0 below 5 do
	(loop for i from 0 below (car dimensions) do
	  (loop for j from 0 below (cadr dimensions) do
	    (setf (aref new-map
			(+ (* tile-i (car dimensions)) i)
			(+ (* tile-j (cadr dimensions)) j))
		  (wrap-around (+ (aref risk-map i j) tile-i tile-j)))))))
    new-map))

;; Then we simply run Dijkstra's algorithm again. (Maybe grab a cup of
;; coffee or something?)
(time (defparameter min-risks (dijkstra '(0 0))))
;;=> Evaluation took:
;;=>   2303.479 seconds of real time
;;=>   2306.343750 seconds of total run time (2069.109375 user, 237.234375 system)
;;=>   [ Run times consist of 165.423 seconds GC time, and 2140.921 seconds non-GC time. ]
;;=>   100.12% CPU
;;=>   5,970,628,356,514 processor cycles
;;=>   1,000,081,900,080 bytes consed
;;=>
;;=> MIN-RISKS

(aref min-risks 499 499)
;;=> 2814
