;;; Code for Advent of Code 2021, Day 9.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 10/12/2021

;;; Problem 1
;; Now we are given as input a 'heightmap' of the seafloor, for which
;; we want to determine the low points:
(ql:quickload '(:uiop :str))
(defparameter seafloor
  (let ((heights (mapcar
		  (lambda (string)
		    (mapcar #'parse-integer
			    (str:split "" string :omit-nulls t)))
		  (uiop:read-file-lines "day9_problem1.txt"))))
    (make-array (list (length heights) (length (car heights)))
		:initial-contents heights)))

;; We want to be able to quickly determine boundary points for a
;; given (i,j) height, so we code some functions to do just this:
(defun adjacent-coordinates (i j)
  "Return the coordinates that are immediately adjacent to (I, J)."
  (let ((adjacents nil)
	(dimensions (array-dimensions seafloor)))
    (if (> i 0) (push (list (1- i) j) adjacents))
    (if (> j 0) (push (list i (1- j)) adjacents))
    (if (< i (1- (car dimensions))) (push (list (1+ i) j) adjacents))
    (if (< j (1- (cadr dimensions))) (push (list i (1+ j)) adjacents))
    adjacents))

(defun adjacent-heights (i j)
  "Return the adjacent heights to point (I, J)."
  (loop for ij in (adjacent-coordinates i j)
	collect
	(apply (lambda (i j) (aref seafloor i j)) ij)))

;; We want to determine the low points in the seafloor, which are
;; defined to be those points which have a lower height than all
;; of its adjacent positions. With the above coded functions, we
;; can trivially write a check for this:
(defun low-point? (i j)
  "True if the point (I, J) is a low point in the seafloor."
  (let ((height (aref seafloor i j))
	(adjacents (adjacent-heights i j))
	(lowestp t))
    (loop for adjacent-height in adjacents do
      (if (<= adjacent-height height)
	  (setf lowestp nil)))
    lowestp))

;; So we can quickly get all of the low points in the seafloor:
(defparameter low-points
  (let ((dimensions (array-dimensions seafloor)))
    (loop for i from 0 below (car dimensions)
	  append
	  (loop for j from 0 below (cadr dimensions)
		when (low-point? i j)
		  collect (list (list i j) (aref seafloor i j))))))

;; Then the risk level of a low-point is simply 1 plus its height, and
;; we sum these up to get our answer.
(reduce #'+ (mapcar (lambda (low-point) (1+ (cadr low-point))) low-points))
;;=> 554


;;; Problem 2
;; Now we are interested in basins, rather than just the low points.
;; We are given (generously) that every basin has exactly one
;; low-point to which all other points flow down to, and so a clever
;; heuristic for finding the basins using the code we've already got
;; is to simply find all of the adjacent points that don't have a
;; height equal to 9 (the largest height), given our list of low-points.
;; So we code a function to do this:
(defun basin (i j)
  "Return all points in the same basin as the point (I, J)."
  (let ((points nil)
	(9-heights nil))
    (labels ((basin-iter (i j)
	       (let ((height (aref seafloor i j)))
		 (push (list i j) points)
		 (if (= height 9)
		     (push (list i j) 9-heights)
		     (loop for ij in (set-difference
				      (adjacent-coordinates i j)
				      points
				      :test #'equalp)
			   do (basin-iter (car ij)
					  (cadr ij)))))))
      (basin-iter i j))
    (remove-duplicates
     (set-difference points 9-heights :test #'equalp)
     :test #'equalp)))

;; Then we simply loop this basin function over all of the low-points
;; to grab the basin sizes:
(defparameter basin-sizes
  (loop for low-point in low-points
	collect
	(length (basin (caar low-point) (cadar low-point)))))

;; And the product of the three largest basin-sizes is
(let ((sorted-sizes (sort (copy-seq basin-sizes) #'>)))
  (* (first sorted-sizes) (second sorted-sizes) (third sorted-sizes)))
;;=> 1017792
