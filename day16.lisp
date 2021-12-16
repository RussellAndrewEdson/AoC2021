;;; Code for Advent of Code 2021, Day 16.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 16/12/2021

;;; Problem 1
;; For our puzzle input we are given a string of hexadecimal digits,
;; which we'll read in:
(ql:quickload '(:uiop :str :alexandria))
(defparameter hexadecimal
  (car (uiop:read-file-lines "day16_problem1.txt")))

;; We want to convert between hexadecimal and binary, so we can
;; define some functions that let us do this conversion:
(defun hexadecimal->binary (number)
  "Convert the given hexadecimal NUMBER string to binary."
  (format nil
	  "~{~a~}"
	  (loop for hexadecimal in (str:split "" number :omit-nulls t)
		collect
		(format nil
			"~4,'0d"
			(parse-integer (write-to-string
					(parse-integer hexadecimal
						       :radix 16)
					:base 2))))))

(defun binary->hexadecimal (number)
  "Convert the given binary NUMBER string to hexadecimal."
  (write-to-string (parse-integer number :radix 2) :base 16))

(hexadecimal->binary "9af")
;;=> "100110101111"

(hexadecimal->binary "111")
;;=> "000100010001"

(binary->hexadecimal "110000100100000001")
;;=> "30901"

;; Now we are given that the hexadecimal string input represents
;; a packet (possibly with trailing zeros), which may contain
;; other packets, and every packet has a header that specifies
;; the version in 3 bits, then the packet type ID in 3 bits, and
;; then the payload. So we can define a function that performs this
;; splitting (that we can use recursively for subpackets):
(defun packet (binary)
  "Return a plist parsing the given BINARY string into packet form."
  (let ((version (parse-integer (str:substring 0 3 binary) :radix 2))
	(type-id (parse-integer (str:substring 3 6 binary) :radix 2))
	(payload (str:substring 6 (length binary) binary)))
    (list :version version
	  :type-id type-id
	  :payload payload)))

(packet (hexadecimal->binary "D2FE28"))
;;=> (:VERSION 6 :TYPE-ID 4 :PAYLOAD "101111111000101000")

;; Now suppose our packet represented a literal (i.e. with a
;; type-id of 4:
(defun literal? (packet)
  "True if the given PACKET encodes a binary literal."
  (equalp (getf packet :type-id) 4))

;; For a literal, we break the payload up into groups of 5
;; (ignoring trailing zeros), where each group of five bits
;; is of the form:
;;    F _ _ _ _
;; where F is a flag for whether this is the last piece of the
;; literal or not (F=1 means it is an intermediate group, F=0
;; means it is the last group), and the rest of the bits form
;; the number.
;;
;; Since the literal stops as soon as we see a zero in the flag,
;; we'll return the rest of the binary string as an additional
;; output just in case.
(defun parse-literal (payload)
  "Return the decimal representation of a given literal PAYLOAD."
  (let ((binary nil)
	(index 0)
	(groups nil))
    (loop for (b1 b2 b3 b4 b5) on (str:split "" payload :omit-nulls t)
	  by (lambda (list) (nthcdr 5 list))
	  when (not (null b5))
	    do
	       (setf groups
		     (append groups
			     (list
			      (list b1 (format nil
					       "~{~a~}"
					       (list b2 b3 b4 b5)))))))
    (loop named amalgamate-groups for (flag bits) in groups do
      (setf index (+ index 5))
      (setf binary (append binary (list bits)))
      (if (equalp flag "0") (return-from amalgamate-groups)))
    (setf binary (format nil "~{~a~}" binary))
    (values (parse-integer binary :radix 2)
	    (str:substring index (length payload) payload))))

(parse-literal "101111111000101000")
;;=> 2021
;;=> "000"

;; And so a general 'parse' function to operate on a general
;; packet (only half-implemented so far) might look like this:
(defun parse (packet)
  "Return the parsed PACKET structure, along with any excess bits."
  (let ((packet (copy-seq packet)))
    (multiple-value-bind (structure excess)
	(cond ((literal? packet) (parse-literal (getf packet :payload)))
	      (t (values (getf packet :payload) "")))
      (setf (getf packet :payload) structure)
      (values packet excess))))

(parse (packet (hexadecimal->binary "D2FE28")))
;;=> (:VERSION 6 :TYPE-ID 4 :PAYLOAD 2021)
;;=> "000"

(parse (packet (hexadecimal->binary "38006F45291200")))
;;=> (:VERSION 1 :TYPE-ID 6 :PAYLOAD
;;=>  "00000000000110111101000101001010010001001000000000")
;;=> ""

;; Every other packet type is an operator, for which we are given (in
;; this first part) that the payload then has the form
;;   L (11-15 bits) _ _ _ _ ...
;; where L is the length-type ID, such that
;;   L=0 means the next 15 bits specify the total bit-length for the
;;     subpackets, which start immediately afterward,
;;   L=1 means the next 11 bits specify the total number of subpackets
;;     contained in this packet, which also start immediately afterward.
(defun operator? (packet)
  "True if the given PACKET encodes an operator."
  (not (equalp (getf packet :type-id) 4)))

(defun parse-operator (payload)
  "Return the parsed operator PAYLOAD (including subpackets)."
  (let* ((parsed-payload nil)
	 (length-type-id (str:substring 0 1 payload))
	 (length-size (if (equalp length-type-id "0") 15 11))
	 (index (1+ length-size))
	 (length-bits (str:substring 1 index payload))
	 (rest-of-payload (str:substring index (length payload) payload)))
    ;; If the length-type-id is 0, we are given the total length
    ;; of the sub-packets.
    (if (equalp length-type-id "0")
	(let ((total-subpacket-bits (parse-integer length-bits :radix 2)))
	  (setf index 0)
	  (loop while (< index total-subpacket-bits) do
	    (multiple-value-bind (subpacket rest)
		(parse (packet rest-of-payload))
	      (setf parsed-payload (append parsed-payload (list subpacket)))
	      (setf index (+ index (- (length rest-of-payload) (length rest))))
	      (setf rest-of-payload rest))))
	;; Otherwise, we are told how many subpackets are included.
	(let ((subpacket-count 0)
	      (total-subpackets (parse-integer length-bits :radix 2)))
	  (loop while (< subpacket-count total-subpackets) do
	    (multiple-value-bind (subpacket rest)
		(parse (packet rest-of-payload))
	      (setf parsed-payload (append parsed-payload (list subpacket)))
	      (incf subpacket-count)
	      (setf rest-of-payload rest)))))
    (values parsed-payload rest-of-payload)))

;; Now we redefine our parse function and add an additional
;; clause to account for operator packets, and everything magically
;; works.
(defun parse (packet)
  "Return the parsed PACKET structure, along with any excess bits."
  (let ((packet (copy-seq packet)))
    (multiple-value-bind (structure excess)
	(cond ((literal? packet) (parse-literal (getf packet :payload)))
	      ((operator? packet) (parse-operator (getf packet :payload)))
	      (t (values (getf packet :payload) "")))
      (setf (getf packet :payload) structure)
      (values packet excess))))

(parse (packet (hexadecimal->binary "38006F45291200")))
;;=> (:VERSION 1 :TYPE-ID 6 :PAYLOAD
;;=>  ((:VERSION 6 :TYPE-ID 4 :PAYLOAD 10) (:VERSION 2 :TYPE-ID 4 :PAYLOAD 20)))
;;=> "0000000"

(parse (packet (hexadecimal->binary "EE00D40C823060")))
;;=> (:VERSION 7 :TYPE-ID 3 :PAYLOAD
;;=>  ((:VERSION 2 :TYPE-ID 4 :PAYLOAD 1) (:VERSION 4 :TYPE-ID 4 :PAYLOAD 2)
;;=>   (:VERSION 1 :TYPE-ID 4 :PAYLOAD 3)))
;;=> "00000"

;; So at this point we can parse our entire given input hexadecimal to
;; uncover the packet hierarchy structure.
(defparameter input-packet
  (parse (packet (hexadecimal->binary hexadecimal))))

;; And for part 1 we simply want to loop over the entire structure
;; and sum up the version numbers.
(loop for (key value) on (alexandria:flatten input-packet)
      when (equalp key :version)
	sum value)


;;; Problem 2
;; Now we are given the types according to the type-id, so we can
;; define (a very Lispy!) 'eval'-style function for our packet
;; structure.
(defun eval-packet (packet)
  "Evaluate the value of the given PACKET."
  (cond ((equalp (getf packet :type-id) 0)
	 (reduce #'+ (mapcar #'eval-packet (getf packet :payload))))
	((equalp (getf packet :type-id) 1)
	 (reduce #'* (mapcar #'eval-packet (getf packet :payload))))
	((equalp (getf packet :type-id) 2)
	 (apply #'min (mapcar #'eval-packet (getf packet :payload))))
	((equalp (getf packet :type-id) 3)
	 (apply #'max (mapcar #'eval-packet (getf packet :payload))))
	((equalp (getf packet :type-id) 4) (getf packet :payload))
	((equalp (getf packet :type-id) 5)
	 (if (> (eval-packet (car (getf packet :payload)))
		(eval-packet (cadr (getf packet :payload))))
	     1
	     0))
	((equalp (getf packet :type-id) 6)
	 (if (< (eval-packet (car (getf packet :payload)))
		(eval-packet (cadr (getf packet :payload))))
	     1
	     0))
	((equalp (getf packet :type-id) 7)
	 (if (= (eval-packet (car (getf packet :payload)))
		(eval-packet (cadr (getf packet :payload))))
	     1
	     0))
	(t "pass")))

(eval-packet (parse (packet (hexadecimal->binary "C200B40A82"))))
;;=> 3

;; So we simply evaluate the entire packet structure.
(eval-packet input-packet)
;;=> 1264857437203
