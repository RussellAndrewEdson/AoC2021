;;; Code for Advent of Code 2021, Day 20.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 20/12/2021

;;; Problem 1
;; We are given as input a string for the 'image enhancement algorithm'
;; as the first line, and then a 100x100 grid of light pixels '#' and
;; dark pixels '.' representing our initial image:
(ql:quickload '(:str :alexandria))

(with-open-file (input "day20_problem1.txt")
  (defparameter image-enhance
    (str:split "" (read-line input nil) :omit-nulls t))
  (read-line input nil)
  (defparameter image
    (loop for line = (read-line input nil)
	  while line
	  collect (str:split "" line :omit-nulls t))))

;; Now the image enhancement works by considering the 3x3 group
;; of pixels that are adjacent to given pixel indices, so we
;; first want to code a function that takes a set of indices and
;; returns the indices for each of the pixels in that 3x3 group:
(defun 3x3-group (ij)
  "Return a list of the 9 indices in the 3x3 group about IJ=(I, J)."
  (destructuring-bind (i j) ij
    (list (list (1- i) (1- j)) (list (1- i) j) (list (1- i) (1+ j))
	  (list i (1- j)) (list i j) (list i (1+ j))
	  (list (1+ i) (1- j)) (list (1+ i) j) (list (1+ i) (1+ j)))))

(3x3-group '(0 0))
;;=> ((-1 -1) (-1 0) (-1 1) (0 -1) (0 0) (0 1) (1 -1) (1 0) (1 1))

;; Now, given a 3x3 group of pixels, the image enhancement works
;; as follows. We get the values of each of the pixels at those
;; indices, and convert them to a binary number. The decimal value
;; of the binary number in the image-enhancement string tells us
;; whether the pixel at (i, j) is replaced with a light pixel '#'
;; or a dark pixel '.'. We'll postpone defining a 'pixel-value'
;; function for now, but assume that it returns either "#" or "."
;; accordingly:
(defun enhance (ij)
  "Return the replacement pixel at IJ=(I, J) in the image."
  (let ((index (parse-integer
		(format nil
			"~{~a~}"
			(mapcar (lambda (pixel)
				  (if (equalp pixel "#") 1 0))
				(loop for indices in (3x3-group ij)
				      collect (pixel-value indices))))
		:radix 2)))
    (elt image-enhance index)))

;; Having coded these functions so far, we can immediately make the
;; observation that if we want to account for "infinitely large"
;; image sizes, then since it is a binary image we can store
;; the image efficiently by simply keeping track of the (i, j)
;; indices for the light pixels, for instance.
;;
;; As to how we account for the image being infinitely large,
;; we can note that the image starts surrounded by infinitely many
;; dark pixels '.', and so the binary index for the enhancement
;; algorithm string will be 0 since every group of 3x3 pixels
;; will be of the form '. . .; . . .; . . .'.
(car image-enhance)
;;=> "#"

;; That is, on every enhancement step all of the infinitely-many
;; dark pixels surrounding the image are turned into light pixels.
;; On the next iteration, there will be infinitely-many groups of
;; 3x3 pixels of the form '# # #; # # #; # # #', which get turned
;; into:
(car (last image-enhance))
;;=> "."

;; So we see that the infinitely many pixels that border the image
;; toggle between light and dark pixels indefinitely. So we can
;; easily account for this in our data representation for the image
;; as follows:
(defparameter image
  (list
   :outer "."
   :xmin 0
   :xmax (1- (length (car image)))
   :ymin 0
   :ymax (1- (length image))
   :light-pixels (loop for i from 0 below (length image)
		       append
		       (loop for j from 0 below (length (elt image i))
			     when (equalp (elt (elt image i) j) "#")
			       collect (list i j)))))

;; So with our decision for the image representation finished, we
;; can define a useful 'pixel-value' function to return "#" or "."
;; without having to check infinitely-many indices.
(defun pixel-value (ij)
  "Return the pixel value of image at index IJ=(I, J)."
  (destructuring-bind (i j) ij
    (cond ((or (< i (getf image :ymin))
	       (> i (getf image :ymax))
	       (< j (getf image :xmin))
	       (> j (getf image :xmax)))
	   (getf image :outer))
	  ((member ij (getf image :light-pixels) :test #'equalp) "#")
	  (t "."))))

(pixel-value '(-1000 -1000))
;;=> "."

(pixel-value '(0 5))
;;=> "#"

(enhance '(-1000 -1000))
;;=> "#"

;; So we can finally code a single step of the image-enhancement
;; algorithm, noting that we extend the image by 2 in each of the
;; cardinal directions (since we consider 3x3 groups for all pixels
;; in the image, not worrying about the infinitely-many pixels that
;; surround the image).
(defun update-image ()
  "Update image by applying the image enhancement algorithm."
  (let ((xmin (- (getf image :xmin) 2))
	(xmax (+ (getf image :xmax) 2))
	(ymin (- (getf image :ymin) 2))
	(ymax (+ (getf image :ymax) 2)))
    (setf image
	  (list
	   :outer (if (equalp (getf image :outer) "#") "." "#")
	   :xmin xmin
	   :xmax xmax
	   :ymin ymin
	   :ymax ymax
	   :light-pixels (loop for i from ymin upto ymax
			       append
			       (loop for j from xmin upto xmax
				     when (equalp (enhance (list i j)) "#")
				       collect (list i j)))))))

;; Finally, we want a way to count the light pixels in the image,
;; which is also straightforward with our image representation:
(defun light-pixels-count ()
  "Return the total number of light pixels in image."
  (if (equalp (getf image :outer) "#")
      :infinity
      (length (getf image :light-pixels))))

(light-pixels-count)
;;=> 4964

;; So after two applications of the image enhancement algorithm:
(update-image)
(light-pixels-count)
;;=> :INFINITY

(update-image)
(light-pixels-count)
;;=> 5765

   
;;; Problem 2
;; Now we simply run the image enhancement algorithm 50 times (or 48
;; more times on top of the iterations in part 1).
(time
 (progn
   (loop repeat 48 do (update-image))
   (light-pixels-count)))
;;=> Evaluation took:
;;=>   3752.810 seconds of real time
;;=>   3750.0000000 seconds of total run time (3749.093750 user, 0.906250 system)
;;=>   [ Run times consist of 0.499 seconds GC time, and 3749.501 seconds non-GC time. ]
;;=>   99.93% CPU
;;=>   9,727,323,481,086 processor cycles
;;=>   2,902,277,168 bytes consed
;;=>
;;=> 18509
