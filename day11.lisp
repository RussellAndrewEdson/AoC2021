;;; Code for Advent of Code 2021, Day 11.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 11/12/2021

;;; Problem 1
;; We are given a 10x10 grid of octopus energy levels for the
;; puzzle input:
(ql:quickload '(:uiop :str :alexandria))
(defparameter energy
  (mapcar (lambda (string)
	    (mapcar #'parse-integer
		    (str:split "" string :omit-nulls t)))
	  (uiop:read-file-lines "day11_problem1.txt")))
(setf energy (make-array (list (length energy) (length (car energy)))
			 :initial-contents energy))

;; For part 1, we are interested in computing octopii flashes, which
;; happen as follows. In a single iteration, every octopus has its
;; energy level increase by 1. Any octopus that has an energy level
;; of 10 or higher flashes, which causes all of the adjacent octopii
;; to have their energy further increased by 1. If they also flash,
;; then the process repeats, etc. Finally, the flashed octopii have
;; their energy levels reset to zero.
;;
;; So we'll need a function to determine adjacent indices given (i,j):
(defun adjacent-indices (i j)
  "Return a list of all indices adjacent to (I, J)."
  (let* ((dimensions (array-dimensions energy))
	 (y-min 0)
	 (y-max (1- (car dimensions)))
	 (x-min 0)
	 (x-max (1- (cadr dimensions)))
	 (indices (list (list (1- i) (1- j))
			(list (1- i) j)
			(list (1- i) (1+ j))
			(list i (1- j))
			(list i (1+ j))
			(list (1+ i) (1- j))
			(list (1+ i) j)
			(list (1+ i) (1+ j)))))
    (remove-if-not
     (lambda (indices)
       (let ((y (car indices))
	     (x (cadr indices)))
	 (and (>= y y-min) (<= y y-max) (>= x x-min) (<= x x-max))))
     indices)))

(adjacent-indices 2 3)
;;=> ((1 2) (1 3) (1 4) (2 2) (2 4) (3 2) (3 3) (3 4))

;; Now we code a function that takes an energy state array
;; and returns the next iteration.
(defun next (energy)
  "Return the next state of the ENERGY values after a single step."
  (let ((new-energy (alexandria:copy-array energy))
	(dimensions (array-dimensions energy)))
    ;; Increment all energies
    (loop for i from 0 below (car dimensions) do
      (loop for j from 0 below (cadr dimensions) do
	(incf (aref new-energy i j))))
    ;; Then check for flashes
    (let ((flashing nil)
	  (flashed nil))
      (loop for i from 0 below (car dimensions) do
	(loop for j from 0 below (cadr dimensions) do
	  (if (> (aref new-energy i j) 9)
	      (push (list i j) flashing))))
      (loop until (null flashing) do
	(let ((octopus (pop flashing)))
	  (push octopus flashed)
	  (loop for adjacent in (adjacent-indices (car octopus)
						  (cadr octopus))
		do
		   (let ((i (car adjacent))
			 (j (cadr adjacent)))
		     (incf (aref new-energy i j))
		     (if (and (> (aref new-energy i j) 9)
			      (not (member (list i j) flashing :test #'equalp))
			      (not (member (list i j) flashed :test #'equalp)))
			 (push (list i j) flashing))))))
      ;; Reset flashed octopii to zero
      (loop for octopus in flashed do
	(setf (aref new-energy (car octopus) (cadr octopus)) 0)))
    new-energy))

energy
;;=> #2A((7 6 1 2 6 4 8 2 1 7)
;;=>     (7 6 1 7 2 3 7 6 7 2)
;;=>     (2 8 5 3 8 7 1 8 3 6)
;;=>     (7 2 1 4 3 6 7 1 3 5)
;;=>     (1 5 3 3 3 6 5 6 1 4)
;;=>     (6 2 5 8 1 7 2 8 6 2)
;;=>     (5 3 7 7 6 7 5 5 8 3)
;;=>     (5 6 1 3 2 6 8 2 7 8)
;;=>     (8 3 8 1 1 3 4 4 6 5)
;;=>     (3 4 4 5 4 2 8 7 3 3))

(next energy)
;;=> #2A((8 7 2 3 7 5 9 3 2 8)
;;=>     (8 7 2 8 3 4 8 7 8 3)
;;=>     (3 9 6 4 9 8 2 9 4 7)
;;=>     (8 3 2 5 4 7 8 2 4 6)
;;=>     (2 6 4 4 4 7 6 7 2 5)
;;=>     (7 3 6 9 2 8 3 9 7 3)
;;=>     (6 4 8 8 7 8 6 6 9 4)
;;=>     (6 7 2 4 3 7 9 3 8 9)
;;=>     (9 4 9 2 2 4 5 5 7 6)
;;=>     (4 5 5 6 5 3 9 8 4 4))

(next (next energy))
;;=> #2A((0 0 6 5 9 8 0 8 6 0)
;;=>     (0 0 8 0 7 9 0 0 0 6)
;;=>     (8 0 0 8 0 0 9 0 8 9)
;;=>     (0 7 5 8 9 0 0 7 7 7)
;;=>     (4 9 7 7 9 0 0 0 6 7)
;;=>     (8 6 0 0 9 0 0 0 0 6)
;;=>     (8 8 0 0 0 0 0 0 0 9)
;;=>     (9 0 7 9 8 0 0 0 0 0)
;;=>     (0 8 0 4 4 9 0 0 0 0)
;;=>     (6 8 7 8 6 6 0 0 9 7))

;; So now we simply iterate for 100 steps, counting the number of
;; flashed octopii after each step as we go.
(let ((flashes 0)
      (dimensions (array-dimensions energy))
      (state energy))
  (loop repeat 100 do
    (setf state (next state))
    (loop for i from 0 below (car dimensions) do
      (loop for j from 0 below (cadr dimensions) do
	(if (zerop (aref state i j))
	    (incf flashes)))))
  flashes)
;;=> 1739


;;; Problem 2
;; For part two, we simply want to know when all of the octopii
;; have flashed in a single step (i.e. they are 'synchronised').
;; We iterate until this happens as follows:
(defun synchronised? (energy)
  "True if the octopii are synchronised, false if not."
  (let ((dimensions (array-dimensions energy))
	(synchp t))
    (loop named outer for i from 0 below (car dimensions) do
      (loop for j from 0 below (cadr dimensions) do
	(if (not (zerop (aref energy i j)))
	    (progn
	      (setf synchp nil)
	      (return-from outer)))))
    synchp))

(let ((turn 0)
      (state energy))
  (loop until (synchronised? state) do
    (incf turn)
    (setf state (next state)))
  (list turn state))
;;=> (324
;;=>  #2A((0 0 0 0 0 0 0 0 0 0)
;;=>      (0 0 0 0 0 0 0 0 0 0)
;;=>      (0 0 0 0 0 0 0 0 0 0)
;;=>      (0 0 0 0 0 0 0 0 0 0)
;;=>      (0 0 0 0 0 0 0 0 0 0)
;;=>      (0 0 0 0 0 0 0 0 0 0)
;;=>      (0 0 0 0 0 0 0 0 0 0)
;;=>      (0 0 0 0 0 0 0 0 0 0)
;;=>      (0 0 0 0 0 0 0 0 0 0)
;;=>      (0 0 0 0 0 0 0 0 0 0)))
