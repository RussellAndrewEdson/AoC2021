;;; Code for Advent of Code 2021, Day 6.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 06/12/2021

;;; Problem 1
;; We are given a list of numbers that model reproducing lanternfish
;; (i.e. the 'internal timers'):
(ql:quickload '(:uiop :str))
(defparameter fish
  (mapcar #'parse-integer
	  (str:split "," (car (uiop:read-file-lines "day6_problem1.txt")))))

;; To simulate the lanternfish reproduction after N days, we simply
;; decrement each of the numbers. Whenever a number hits zero, it
;; is reset at 6 and it spawns a new timer starting from 8.
(defun iter-day (timers)
  "Simulate a day passing for the given lanternfish TIMERS."
  (setf timers (mapcar #'1- timers))
  (let ((new-timers nil))
    (loop for index from 0 below (length timers) do
      (if (< (elt timers index) 0)
	  (progn
	    (setf (elt timers index) 6)
	    (push 8 new-timers))))
    (append timers new-timers)))

;; To get the lanternfish population after 80 days, we simply
;; iterate this day-simulation that many times.
(time
 (defparameter 80-day-population
   (let ((population fish))
     (loop repeat 80 do
       (setf population (iter-day population)))
     population)))
;;=> Evaluation took:
;;=>   400.567 seconds of real time
;;=>   400.359375 seconds of total run time (400.234375 user, 0.125000 system)
;;=>   [ Run times consist of 0.079 seconds GC time, and 400.281 seconds non-GC time. ]
;;=>   99.95% CPU
;;=>   1,038,274,209,479 processor cycles
;;=>   130,439,168 bytes consed

(length 80-day-population)
;;=> 358214


;;; Problem 2
;; It is obvious that our naive approach of simulating the lanternfish
;; population one-by-one won't work if we want the 256-day population.
;; Instead, we'll use some mathematics to try and simplify the problem.

;; Consider the newly-hatched lanternfish (i.e. the fish with timer=8).
;; Let N_m denote the number of fish that this fish spawns in m days.
;; Then recursively:
;;   N_m = 1 + N_{m-9}
;;                      + 1 + N_{m-9-7}
;;                                       + 1 + N_{m-9-7-7}
;;                                                          + ...
;; and so on, where N_{m-9}, N_{m-9-7} are the fish spawned by the
;; children of the original fish, time-lagged to their birth.
;; As complex as it is, we can code this up relatively easily as
;; a recursive function (and for efficiency, we'll memoize this):
(let ((memo nil))
  (defun 8-timer-spawns (days)
    "Return the number of fish spawned by an 8-timer fish after DAYS."
    (let ((memoized-index (position days memo :key #'car)))
      (if memoized-index
	  (cadr (elt memo memoized-index))
	  (let ((n (if (< days 9)
		       0
		       (loop for next-days = (- days 9) then (- next-days 7)
			     while (> next-days 0)
			     summing (1+ (8-timer-spawns next-days))))))
	    (push (list days n) memo)
	    n)))))

(time (8-timer-spawns 256))
;;=> Evaluation took:
;;=>   0.002 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   0.00% CPU
;;=>   5,510,720 processor cycles
;;=>   0 bytes consed
;;=>
;;=> 3053201611

;; Now, our trick here will be to 'time-shift' the original lanternfish
;; so that they all start out as 8-timer fish. That is, instead of
;; looking for the number of fish spawned in 256 days by a fish with
;; timer=2, we'll consider the number of fish spawned in 256+7 days
;; with 'timer=8', which lets us use our memoized code. We can test
;; our strategy for the 80-day case to make sure everything is working
;; correctly:
(loop for fish-timer in fish
      summing
      (1+ (8-timer-spawns (+ 80 (- 9 fish-timer)))))
;;=> 358214

;; So the number of fish spawned after 256 days is
(time (loop for fish-timer in fish
	    summing
	    (1+ (8-timer-spawns (+ 256 (- 9 fish-timer))))))
;;=> Evaluation took:
;;=>   0.002 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   0.00% CPU
;;=>   3,011,174 processor cycles
;;=>   0 bytes consed
;;=>
;;=> 1622533344325
