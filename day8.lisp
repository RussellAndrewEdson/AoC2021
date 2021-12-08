;;; Code for Advent of Code 2021, Day 8.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 08/12/2021

;;; Problem 1
;; For this problem we are given our input as strings containing
;; the 10 unique signal patterns for a reading, and then the state
;; of the 7-segment display, delimited by a pipe (|).
(ql:quickload '(:uiop :str))
(defparameter file-text
  (mapcar (lambda (string) (str:split " | " string))
	  (uiop:read-file-lines "day8_problem1.txt")))
(defparameter patterns
  (mapcar (lambda (string) (str:split " " (car string))) file-text))
(defparameter outputs
  (mapcar (lambda (string) (str:split " " (cadr string))) file-text))

;; Now we want to try and decode the 7-segment displays. We can
;; uniquely determine the 7-segs that have 2, 3, 4 or 7 segments
;; lit up, since these correspond unambiguously to 1, 7, 4 and 8
;; respectively. Part 1 asks us to count each of these in the
;; outputs.
(loop for displays in outputs
      summing
      (loop for 7seg in displays
	    for segments = (length 7seg)
	    when (or (= segments 2)
		     (= segments 3)
		     (= segments 4)
		     (= segments 7))
	      count segments))
;;=> 284


;;; Problem 2
;; Now we want to use the signal patterns to properly deduce each
;; of the digits in the 7-segment display output. We can do this as
;; follows. First, we'll code up some helper functions to perform the
;; (set-)differencing:
(defun sort-string (string)
  "Sort the given STRING so that each character appears in alphabet order."
  (sort (copy-seq string) #'string<=))

(defun segment-diff (segment1 segment2)
  "Return the segments in SEGMENT1 that don't appear in SEGMENT2."
  (sort-string (coerce (set-difference (coerce segment1 'list)
				       (coerce segment2 'list))
		       'string)))

(defun extract-n-segments (patterns n)
  "Return those PATTERNS with exactly N segments."
  (remove-if-not (lambda (pattern) (= (length pattern) n)) patterns))

;; Now consider the following pattern:
(defparameter test-pattern (elt patterns 0))
test-pattern
;;=> ("ceg" "gedcfb" "ec" "eabfdg" "gcdabe" "baged" "cabgf" "gbaec"
;;=>  "fecagdb" "eacd")

;; We know that the string "ec" has to correspond to the two right-most
;; segments that make up the digit '1', since that is the only display
;; that has exactly 2 segments lit up. Similarly, "ceg" must
;; correspond to the '7' digit, since only 7 has exactly 3 segments lit
;; up. So we've actually deduced that g is the top-most segment in the
;; swapped configuration.
;; Or that is,
;;
;; 1) 7\1 is the 'a' segment.
(segment-diff (car (extract-n-segments test-pattern 3))
	      (car (extract-n-segments test-pattern 2)))
;;=> "g"

;; Using similar differences and differences of sets, we can deduce the
;; rest of the segments by iterated comparison as follows:
;; i.e.
;;
;; 2. 8\{0,6,9} (i.e. the 6-segment digits) gives us {c,d,e} in
;;      some order to be determined.
(let ((6segs (extract-n-segments test-pattern 6)))
  (mapcar (lambda (6seg) (segment-diff "abcdefg" 6seg)) 6segs))
;;=> ("a" "c" "f")

;; 3. Since we know that the 2-segment pattern is 1=cf, then
;;      f is the segment that we didn't compute in step 2.
(car (extract-n-segments
      (mapcar (lambda (segment)
		(segment-diff (car (extract-n-segments test-pattern 2))
			      segment))
	      '("a" "c" "f"))
      1))
;;=> "e"

;; 4. Then since we know that 7=acf, and we know a and f, then
;;      the leftover segment must be c.
(segment-diff (car (extract-n-segments test-pattern 3))
	      (format nil "~{~a~}" (list "g" "e")))
;;=> "c"

;; 5. Now we take all of the (set-)differences of the 5-segments
;;      and the 6-segments. All of these differences will either
;;      be a single character or nil: of the characters, d is
;;      whichever appears 3 times, and e is whichever appears
;;      exactly once.
(let ((5segs-6segs
	(loop for 5seg in (extract-n-segments test-pattern 5)
	      append
	      (loop for 6seg in (extract-n-segments test-pattern 6)
		    collect
		    (segment-diff 5seg 6seg)))))
  (print 5segs-6segs)
  (loop for segment in (remove-if (lambda (string) (equalp string ""))
				  (remove-duplicates 5segs-6segs
						     :test #'equalp))
	collect
	(cons segment (count segment 5segs-6segs :test #'equalp))))
;;=> ("a" "" "" "a" "c" "f" "a" "c" "")
;;=> (("f" . 1) ("a" . 3) ("c" . 2))

;; 6. Then since 4=bcdf, then b is whatever segment that we haven't
;;      already determined in earlier steps.
(segment-diff (car (extract-n-segments test-pattern 4))
	      (format nil "~{~a~}" (list "g" "e" "c" "a" "f")))
;;=> "d"


;; 7. Finally, the g segment is whatever segment is left over.
(segment-diff "abcdefg" (format nil "~{~a~}" (list "g" "e" "c" "a" "f" "d")))
;;=> "b"

;; So we can code this up in a function given the patterns, which
;; returns a function that translates the corresponding output
;; appropriately.
(defun make-mapping (pattern)
  "Return a function that 'de-scrambles' 7-segments according to PATTERN."
  (let ((mapping (pairlis '("a" "b" "c" "d" "e" "f" "g")
			  '(nil nil nil nil nil nil nil)))
	(2segs (extract-n-segments pattern 2))
	(3segs (extract-n-segments pattern 3))
	(4segs (extract-n-segments pattern 4))
	(5segs (extract-n-segments pattern 5))
	(6segs (extract-n-segments pattern 6))
	;;(7segs (extract-n-segments pattern 7))
	a
	b
	c
	d
	e
	f
	g
	cde
	5segs-6segs)
    (flet ((update-mapping (key value)
	     (setf (cdr (assoc key mapping :test #'equalp)) value)))
      
      ;; 1. 7\1 is the 'a' segment.
      (setf a (segment-diff (car 3segs) (car 2segs)))
      
      ;; 2. 8\{0,6,9} (i.e. the 6-segment digits) gives us {c,d,e} in
      ;;      some order to be determined.
      (setf cde (mapcar (lambda (6seg) (segment-diff "abcdefg" 6seg)) 6segs))
      
      ;; 3. Since we know that the 2-segment pattern is 1=cf, then
      ;;      f is the segment that we didn't compute in step 2.
      (setf f (car (extract-n-segments
		    (mapcar (lambda (segment)
			      (segment-diff (car 2segs) segment))
			    cde)
		    1)))
      
      ;; 4. Then since we know that 7=acf, and we know a and f, then
      ;;      the leftover segment must be c.
      (setf c (segment-diff (car 3segs) (format nil "~{~a~}" (list a f))))
      
      ;; 5. Now we take all of the (set-)differences of the 5-segments
      ;;      and the 6-segments. All of these differences will either
      ;;      be a single character or nil: of the characters, d is
      ;;      whichever appears 3 times, and e is whichever appears
      ;;      exactly once.
      (setf 5segs-6segs
	    (loop for 5seg in 5segs
		  append
		  (loop for 6seg in 6segs
			collect
			(segment-diff 5seg 6seg))))
      (let ((counts (loop for segment in (remove-if
					  (lambda (string) (equalp string ""))
					  (remove-duplicates 5segs-6segs
							     :test #'equalp))
			  collect
			  (cons segment
				(count segment 5segs-6segs :test #'equalp)))))
	(setf d (car (rassoc 3 counts)))
	(setf e (car (rassoc 1 counts))))
      
      ;; 6. Then since 4=bcdf, then b is whatever segment that we haven't
      ;;      already determined in earlier steps.
      (setf b (segment-diff (car 4segs)
			    (format nil "~{~a~}" (list a f c d e))))
      
      ;; 7. Finally, the g segment is whatever segment is left over.
      (setf g (segment-diff "abcdefg" (format nil "~{~a~}" (list a f c d e b))))
      
      (update-mapping a "a")
      (update-mapping b "b")
      (update-mapping c "c")
      (update-mapping d "d")
      (update-mapping e "e")
      (update-mapping f "f")
      (update-mapping g "g")
      
      (flet ((map-segment (segment)
	       (cdr (assoc segment mapping :test #'equalp))))
	(lambda (7seg)
	  (format nil
		  "~{~a~}"
		  (mapcar #'map-segment
			  (mapcar #'string (coerce 7seg 'list)))))))))

;; With this behemoth of a function complete, we now simply map this
;; function over each of the 7-segment digits in the output to
;; determine the correct digits.
(defun decode-7seg-display (pattern output)
  "Return the decoded number from the 7-seg OUTPUT (given PATTERNS)."
  (flet ((digit (7seg)
	   (let ((segments (sort-string 7seg)))
	     (cond ((string= segments "abcefg") "0")
		   ((string= segments "cf") "1")
		   ((string= segments "acdeg") "2")
		   ((string= segments "acdfg") "3")
		   ((string= segments "bcdf") "4")
		   ((string= segments "abdfg") "5")
		   ((string= segments "abdefg") "6")
		   ((string= segments "acf") "7")
		   ((string= segments "abcdefg") "8")
		   ((string= segments "abcdfg") "9")
		   (t nil)))))
    (let ((decoder (make-mapping pattern)))
      (values (parse-integer
	       (format nil "~{~a~}"
		       (loop for 7seg in output
			     collect (digit (funcall decoder 7seg)))))))))

;; Finally, we simply sum up all of the four-digit numbers to get
;; our answer.
(defparameter decoded-values
  (mapcar #'decode-7seg-display patterns outputs))

(reduce #'+ decoded-values)
;;=> 973499
