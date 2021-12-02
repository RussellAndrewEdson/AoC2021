;;; Code for Advent of Code 2021, Day 2.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 02/12/2021

;;; Problem 1
;; We are given our 'submarine commands' as a list of inputs:
(require :uiop)
(ql:quickload :str)
(defparameter commands
  (mapcar
   (lambda (command) (str:split " " command))
   (uiop:read-file-lines "day2_problem1.txt")))

;; We start at depth=0 and horizontal position=0, and
;; maneuver the submarine according to the commands
(defparameter depth 0)
(defparameter horizontal-position 0)

(loop for command in commands do
  (let ((direction (car command))
	(distance (parse-integer (cadr command))))
    (cond ((equalp direction "forward")
	   (setf horizontal-position (+ horizontal-position distance)))
	  ((equalp direction "up")
	   (setf depth (- depth distance)))
	  ((equalp direction "down")
	   (setf depth (+ depth distance)))
	  (t "pass"))))

;; Finally, we multiply the final horizontal position by the
;; depth to get our answer.
(* depth horizontal-position)
;;=> 1636725


;;; Problem 2
;; Now we incorporate the 'aim' of the submarine, which modifies the
;; way the depth and horizontal direction are computed.
(defparameter aim 0)
(defparameter depth 0)
(defparameter horizontal-position 0)

(loop for command in commands do
  (let ((direction (car command))
	(distance (parse-integer (cadr command))))
    (cond ((equalp direction "forward")
	   (setf horizontal-position (+ horizontal-position distance))
	   (setf depth (+ depth (* aim distance))))
	  ((equalp direction "up")
	   (setf aim (- aim distance)))
	  ((equalp direction "down")
	   (setf aim (+ aim distance)))
	  (t "pass"))))

;; Again, we multiply the final depth by the final horizontal
;; position to get our answer.
(* depth horizontal-position)
;;=> 1872757425
