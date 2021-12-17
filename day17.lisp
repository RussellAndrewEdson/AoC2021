;;; Code for Advent of Code 2021, Day 17.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 17/12/2021

;;; Problem 1
;; This time we have a ballistics problem, where we aim to
;; launch a probe into an underwater trench, where the trench's
;; target area is given by the puzzle input.
(ql:quickload '(:uiop :cl-ppcre))
(defparameter trench-target
  (mapcar (lambda (string) (parse-integer string :junk-allowed t))
	  (cdr (cl-ppcre:split "(x=)|(y=)|(\\.\\.)"
			       (car (uiop:read-file-lines
				     "day17_problem1.txt"))))))

(defun in-trench? (x y)
  "True if the given (X, Y) coordinates are inside the trench."
  (and (>= x (car trench-target))
       (<= x (cadr trench-target))
       (>= y (caddr trench-target))
       (<= y (cadddr trench-target))))

;; The trench coordinates actually define the bounds of our
;; search space too: if the x position of our probe ever exceeds
;; the righthand side of the trench, then we can stop. Similarly
;; if the y position ever exceeds the bottom of the trench.
(defparameter xmax (cadr trench-target))
(defparameter ymin (caddr trench-target))

;; Now the probe starts at (x,y) = (0,0) with a given velocity,
;; and flies in the direction of that velocity at each step
;; (but with drag and gravity modifying the velocity over time).
;; So we can code a trajectory function as follows:
(defun trajectory (vx vy)
  "Return the trajectory fired from (0, 0) with velocities (VX, VY)."
  (loop for vxx = vx then (+ vxx (cond ((< vxx 0) 1) ((> vxx 0) -1) (t 0)))
	and vyy = vy then (- vyy 1)
	and x = 0 then (+ x vxx)
	and y = 0 then (+ y vyy)
	while (and (<= x xmax) (>= y ymin))
	collect (list x y)))

(trajectory 10 1)
;;=> ((0 0) (10 1) (19 1) (27 0) (34 -2) (40 -5) (45 -9) (49 -14)
;;=>  (52 -20) (54 -27) (55 -35) (55 -44) (55 -54) (55 -65)
;;=>  (55 -77) (55 -90))

;; We can test whether the trajectory makes it to the trench,
;; starting from the end of the trajectory list for efficiency:
(defun hits-trench? (trajectory)
  "True if the given TRAJECTORY ends up in the trench at some step."
  (loop for (x y) in (reverse trajectory)
	thereis (in-trench? x y)))

(hits-trench? (trajectory 20 10))
;;=> T

;; And given a trajectory, we want to find the maximum height
;; one, which we can get (naively) by just grabbing the maximum
;; y value for each point in the trajectory.
(defun maximum-y (trajectory)
  "Return the maximum y position attained for this TRAJECTORY."
  (loop for (x y) in trajectory maximizing y))

(maximum-y (trajectory 20 10))
;;=> 55

;; However, there are still a huge number of trajectories that
;; we would need to search if we want to check them all, so we'll
;; use some mathematics to cut down on the search space.
;;
;; First note that since we start from (0, 0) and we're interested
;; in shooting as high as possible (so safely assume that we're only
;; looking for positive initial y-velocities). So without
;; having to think too hard about it, this already suggests some
;; reasonable bounds on our initial velocities:
;;   1. Since the absolute velocity at y=0 on the down-swing will
;;      typically be larger than or equal to the initial y-velocity,
;;      we need that to be roughly the order of the vertical size of
;;      the trench if we're to have any hope of landing the shot
;;      without just shooting over it. So a sensible maximum
;;      y-velocity for our given trench is something like vymax = 100.
;;      (Any higher than this and we're certain to overshoot the
;;      trench immediately when the probe falls below the y=0 axis,
;;      regardless of what our x-velocity is.)
(defparameter vymax 100)

;;   2. We need at least 4 points in the trajectory (the y=0 points,
;;      a maximum height and the one that ends in the trench). So
;;      this gives us a sensible bound for vxmax too; since it
;;      decreases by roughly one each step, then vxmax = 60 or so
;;      sounds reasonable. (Again, any higher than that and we
;;      basically overshoot the trench immediately.)
(defparameter vxmax 60)

;; This is a much smaller search space, and one that we could
;; exhaustively search over to find the maximum height trajectory.
;; So we do just that:
(time
 (let ((max-height 0))
   (loop for vx from 1 upto vxmax do
     (loop for vy from 1 upto vymax do
       (let ((this-trajectory (trajectory vx vy)))
	 (if (hits-trench? this-trajectory)
	     (let ((height (maximum-y this-trajectory)))
	       (if (> height max-height)
		   (setf max-height height)))))))
   max-height))
;;=> Evaluation took:
;;=>   0.007 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   0.00% CPU
;;=>   19,335,694 processor cycles
;;=>   15,595,408 bytes consed
;;=>
;;=> 4851


;;; Problem 2
;; Now we are interested in the total number of initial velocities
;; that will end in the trench (e.g. note that we can shoot
;; 'downwards' now). This is another straightforward brute-force
;; search, although note now that our above bounds need to be
;; extended (here we have a vymin to consider, and vxmax increases
;; too).
;;
;; Again, we posit some simple but effective upper bounds. We need
;; vxmax to absolutely be less than 230 (otherwise we overshoot
;; on the very first step), and we need vymin = -100 (since we cannot
;; go lower than that without overshooting the trench).
(defparameter vymin -100)
(defparameter vymax 100)
(defparameter vxmax 230)

;; So the number of trajectories is:
(time
 (loop for vx from 1 upto vxmax
       summing
       (loop for vy from vymin upto vymax
	     when (hits-trench? (trajectory vx vy))
	       count 1)))
;;=> Evaluation took:
;;=>   0.012 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   0.00% CPU
;;=>   32,963,059 processor cycles
;;=>   22,639,952 bytes consed
;;=>
;;=> 1739
