;;; Code for Advent of Code 2021, Day 4.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 07/12/2021

;;; Problem 1
;; We are given a list of bingo numbers, as well as a collection of
;; bingo boards in the input text that we need to parse.
(ql:quickload '(:str :alexandria))

(with-open-file (input "day4_problem1.txt")
  ;; The first line is the bingo numbers
  (defparameter bingo-numbers
    (mapcar #'parse-integer
	    (str:split "," (read-line input nil))))
  (read-line input nil)
  ;; The third line onwards defines the bingo boards
  (defparameter bingo-boards '())
  (loop for line = (read-line input nil)
	while line
	do
	   (let ((board (list (mapcar #'parse-integer (str:words line)))))
	     (loop for line = (read-line input nil)
		   while (not (or (equalp line "") (null line)))
		   do
		      (setf board
			    (append board
				    (list (mapcar #'parse-integer
						  (str:words line))))))
	     (setf bingo-boards (append bingo-boards (list board))))))

;; We attack this problem in stages.
;; First, we want to define a function to check for a winning bingo
;; board (suppose that we mark the called numbers with X). So we want
;; to check every row and every column to see if we have a winner.
;; We'll define some helper functions here too:
(defun transpose (board)
  "Return the row->column transpose for this BOARD."
  (apply #'mapcar #'list board))

(defun all-Xs? (row)
  "True if the given row is all Xs, and false otherwise."
  (null (remove-if (lambda (element) (equalp "X" element)) row)))

(defun bingo? (board)
  "True if this BOARD has a bingo, false otherwise."
  (let ((bingo nil))
    (loop for row in board do
      (if (all-Xs? row)
	  (setf bingo t)))
    (loop for column in (transpose board) do
      (if (all-Xs? column)
	  (setf bingo t)))
    bingo))

;; Next, we'll want a function that takes a board and a number and
;; returns the row and column index for that number if it exists
;; (or NIL otherwise):
(defun find-number (board number)
  "Return the row and column for NUMBER in the given BOARD."
  (let ((row-col nil))
    (loop for row-number from 0 below (length board) do
      (let ((col-number (position number (elt board row-number))))
	(if col-number
	    (setf row-col (list row-number col-number)))))
    row-col))

;; Then we define a function that takes a given bingo board and
;; returns the turn number at which "Bingo!" is called (for
;; convenience, we'll also return the marked board):
(defun bingo-turn (board numbers)
  "Return the winning turn for this BOARD with the called NUMBERS."
  (let ((board (copy-tree board))
	(turn-number 0))
    (loop for number in numbers do
      (incf turn-number)
      ;; Mark the number on the board (if it exists)
      (let ((number-position (find-number board number)))
	(if number-position
	    (setf (elt (elt board (car number-position))
		       (cadr number-position))
		  "X")))
      ;; Check for a bingo.
      (if (bingo? board)
	  (return (values turn-number board))))))

;; Now, we want to know the score of a winning bingo board, which
;; is the sum of all of the leftover numbers upon a bingo, multiplied
;; by the number that was just called on the turn number for the win.
(defun score (board numbers)
  "Return the score of this bingo BOARD with the called NUMBERS."
  (multiple-value-bind (win-turn win-board) (bingo-turn board numbers)
    (* (elt numbers (1- win-turn))
       (reduce #'+ (loop for row in win-board
			 append
			 (remove-if
			  (lambda (element) (equalp element "X")) row))))))

;; So for our answer we want to know which board wins first:
(alexandria:extremum 
 (loop for i from 0 below (length bingo-boards)
       collect
       (list i (bingo-turn (elt bingo-boards i) bingo-numbers)))
 #'<
 :key #'cadr)
;;=> (11 22)

;; So the 11th board in the list (indexed from 0) wins on turn 22,
;; with score:
(score (elt bingo-boards 11) bingo-numbers)
;;=> 23177


;;; Problem 2
;; Now we want to know which board wins last, and the final score for
;; that board. With the above apparatus set up, finding the last board
;; to win is a trivial copy-paste affair with a single character change:
(alexandria:extremum 
 (loop for i from 0 below (length bingo-boards)
       collect
       (list i (bingo-turn (elt bingo-boards i) bingo-numbers)))
 #'>
 :key #'cadr)
;;=> (59 87)

;; With score:
(score (elt bingo-boards 59) bingo-numbers)
;;=> 6804
