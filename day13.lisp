;;; Code for Advent of Code 2021, Day 13.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 13/12/2021

;;; Problem 1
;; Here our input comes in two parts (separated by a blank line):
;; first a list of coordinates, and then the list of origami-style
;; folds to make.
(ql:quickload '(:str))

(with-open-file (input "day13_problem1.txt")
  (defparameter dots
    (loop for line = (read-line input nil)
	  while (not (equalp line ""))
	  collect
	  (mapcar #'parse-integer (str:split "," line))))
  (defparameter folds
    (loop for line = (read-line input nil)
	  while line
	  collect
	  (car (last (str:split " " line))))))

;; Now we'll represent the transparent sheet full of dots by a 2D
;; array, assuming that the spatial extent of the paper is exactly
;; wide enough to show all the dots and no wider:
(defparameter xmin 0)
(defparameter xmax (apply #'max (mapcar #'car dots)))
(defparameter ymin 0)
(defparameter ymax (apply #'max (mapcar #'cadr dots)))

(defparameter sheet
  (make-array (list (1+ xmax) (1+ ymax)) :initial-element 0))
(loop for dot in dots do
  (setf (aref sheet (car dot) (cadr dot)) 1))

;; We're interested in counting dots (after folds), so we'll
;; prepare a function to do this count for us now:
(defun count-dots (sheet)
  "Return the dot count for the given sheet."
  (destructuring-bind (xdim ydim) (array-dimensions sheet)
    (loop for x from 0 below xdim
	  summing
	  (loop for y from 0 below ydim
		when (/= (aref sheet x y) 0)
		  count y))))

(count-dots sheet)
;;=> 836

;; Now we want to implement the fold (in as general a way as possible).
;; e.g. Given a fold up at y = y', for instance, then we end up with
;; a smaller array from 0 to y', with all of the points below y' mapped
;; the same distance above it (so the row at y'+1 gets mapped to y'-1,
;; y'+2 gets mapped to y'-2, and so on until y'+(ymax-y').
;;
;; We'll simply add overlapping dots when we find them (rather than
;; ensuring a binary matrix for sheet).
(defun fold-up (sheet y)
  "Return a new sheet after folding SHEET up from y=Y."
  (destructuring-bind (xdim ydim) (array-dimensions sheet)
    (let ((new-sheet (make-array (list xdim y) :initial-element 0)))
      ;; Fill in the dots above the fold
      (loop for x from 0 below xdim do
	(loop for yy from 0 below y do
	  (if (> (aref sheet x yy) 0) (incf (aref new-sheet x yy)))))
      ;; Fill in the dots below the fold
      (loop for x from 0 below xdim do
	(loop for yy from 0 below y do
	  (if (> (aref sheet x (- ydim 1 yy)) 0) (incf (aref new-sheet x yy)))))
      new-sheet)))

;; Similarly, we fold left for the x=x' folds:
(defun fold-left (sheet x)
  "Return a new sheet after folding SHEET left from x=X."
  (destructuring-bind (xdim ydim) (array-dimensions sheet)
    (let ((new-sheet (make-array (list x ydim) :initial-element 0)))
      ;; Fill in the dots to the left of the fold
      (loop for xx from 0 below x do
	(loop for y from 0 below ydim do
	  (if (> (aref sheet xx y) 0) (incf (aref new-sheet xx y)))))
      ;; Fill in the dots to the right of the fold
      (loop for xx from 0 below x do
	(loop for y from 0 below ydim do
	  (if (> (aref sheet (- xdim 1 xx) y) 0) (incf (aref new-sheet xx y)))))
      new-sheet)))

;; And now we can make a general 'fold' function that takes in a fold
;; instruction and performs the fold.
(defun fold (sheet instruction)
  "Return the new sheet after folding SHEET according to INSTRUCTION."
  (destructuring-bind (direction coordinate) (str:split "=" instruction)
    (cond ((equalp direction "x") (fold-left sheet (parse-integer coordinate)))
	  ((equalp direction "y") (fold-up sheet (parse-integer coordinate)))
	  (t nil))))

;; So performing the first fold, we get
(defparameter first-fold (fold sheet (elt folds 0)))
(count-dots first-fold)
;;=> 720


;;; Problem 2
;; If we finish folding the paper, we get:
(defparameter fully-folded
  (let ((folded-sheet sheet))
    (loop for next-fold in folds do
      (setf folded-sheet (fold folded-sheet next-fold)))
    folded-sheet))

;; And to help us read the answer, we can map to '#' and '.' characters:
(defun print-array (array)
  "Pretty-print the given ARRAY so we can see the message."
  (format t
	  "~{~{~a~}~%~}"
	  (destructuring-bind (xdim ydim) (array-dimensions array)
	    (loop for y from 0 below ydim
		  collect
		  (loop for x from 0 below xdim
			append (list (if (> (aref array x y) 0) "#" ".")))))))

(print-array fully-folded)
;;=> .##..#..#.###..###..###...##..#..#.####.
;;=> #..#.#..#.#..#.#..#.#..#.#..#.#..#....#.
;;=> #..#.####.#..#.#..#.#..#.#..#.#..#...#..
;;=> ####.#..#.###..###..###..####.#..#..#...
;;=> #..#.#..#.#....#.#..#....#..#.#..#.#....
;;=> #..#.#..#.#....#..#.#....#..#..##..####.
;;=> NIL
