;;; Code for Advent of Code 2021, Day 14.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 14/12/2021

;;; Problem 1
;; We are given as input the polymer template (on the first line)
;; and the list of pair insertion rules (for the rest of the lines):
(ql:quickload '(:str :alexandria))

(with-open-file (input "day14_problem1.txt")
  (defparameter polymer-template (read-line input nil))
  (read-line input nil)
  (defparameter pair-insertion-rules
    (loop for line = (read-line input nil)
	  while line
	  collect line)))

;; The pair insertion rules tell us how to modify the given
;; template by inserting a character between two adjacent
;; characters wherever they appear. The rules are all applied
;; simultaneously to each adjacent pair of characters in the
;; polymer template, so we first want a function that splits
;; the template into adjacent pairs:
(defun split-into-pairs (string)
  "Splits the given STRING into adjacent character pairs."
  (loop for (a b) on (str:split "" string :omit-nulls t)
	until (null b)
	collect (format nil "~a~a" a b)))

(split-into-pairs "NNCB")
;;=> ("NN" "NC" "CB")

;; We'll want a function that goes the other way too: given
;; a list of strings, amalgamates them back into a single string:
(defun amalgamate-strings (strings)
  "Recombines STRINGS into a single string (removing redundant pairs)."
  (format nil
	  "~{~a~}"
	  (cons (car strings)
		(loop for string in (cdr strings)
		      collect
		      (str:substring 1 (length string) string)))))

(amalgamate-strings '("ABC" "CDE" "EFG"))
;;=> "ABCDEFG"

;; Now we want to code a function that, given a pair, applies the
;; necessary insertion rule based on the rules list. We can do that
;; with a hash table (for example) as follows:
(defparameter pair-insertion (make-hash-table :test #'equalp))
(loop for rule in pair-insertion-rules do
  (destructuring-bind (pair insertion) (str:split " -> " rule)
    (setf (gethash pair pair-insertion)
	  (format nil
		  "~{~a~}"
		  (list (str:substring 0 1 pair)
			insertion
			(str:substring 1 2 pair))))))

(defun pair-insertion (pair)
  "Return PAIR with the appropriate character inserted according to the rules."
  (gethash pair pair-insertion))

;; Finally, we can code a convenient function that loops over all
;; of the pairs in a given string and performs all of the insertions
;; simultaneously.
(defun apply-rules (string)
  "Apply the pair insertion rules to the given STRING."
  (amalgamate-strings
   (loop for pair in (split-into-pairs string) collect (pair-insertion pair))))

polymer-template
;;=> "OOBFPNOPBHKCCVHOBCSO"

(apply-rules polymer-template)
;;=> "OKOSBCFSPBNHONPFBKHFKHCFCFVCHNOSBSCNSFO"

;; Now we want to keep track of the counts of each character in the
;; string, so we code another function to do that:
(defun count-characters (string)
  "Return a count of each of the characters in the given STRING."
  (let ((characters (str:split "" string :omit-nulls t)))
    (loop for character in (remove-duplicates characters :test #'equalp)
	  collect
	  (list character
		(count character characters :test #'equalp)))))

(count-characters polymer-template)
;;=> (("F" 1) ("N" 1) ("P" 2) ("K" 1) ("V" 1) ("H" 2) ("B" 3)
;;=>  ("C" 3) ("S" 1) ("O" 5))

;; And given this list-pair representation, we can easily find
;; the most common and least common elements and their counts:
(defun most-common (counts)
  "Return the most common character from COUNTS."
  (alexandria:extremum counts #'> :key #'cadr))

(defun least-common (counts)
  "Return the least common character from COUNTS."
  (alexandria:extremum counts #'< :key #'cadr))

;; So: we perform 10 rule applications to the polymer template,
;; find the most common and least common characters, and subtract
;; their occurrences to get our result.
(defparameter modified-polymer polymer-template)
(time
 (loop repeat 10 do (setf modified-polymer (apply-rules modified-polymer))))
;;=> Evaluation took:
;;=>   0.020 seconds of real time
;;=>   0.015625 seconds of total run time (0.015625 user, 0.000000 system)
;;=>   80.00% CPU
;;=>   51,798,335 processor cycles
;=>   14,004,192 bytes consed
;;=>
;;=> NIL

(defparameter char-counts (count-characters modified-polymer))
char-counts
;;=> (("N" 1215) ("H" 1966) ("B" 1900) ("P" 1154) ("C" 2285)
;;=>  ("V" 1485) ("K" 3077) ("F" 3115) ("S" 2339) ("O" 921))

(length modified-polymer)
;;=> 19457

(most-common char-counts)
;;=> ("F" 3115)

(least-common char-counts)
;;=> ("O" 921)

(- (cadr (most-common char-counts)) (cadr (least-common char-counts)))
;;=> 2194


;;; Problem 2
;; For part 2, we want to extend to 40 iterations. However, the problem
;; here is that our polymer grows ridiculously long very quickly, and
;; we'll quickly exhaust all of our memory (and time) trying to find the
;; character counts if we try to explicitly create the modified polymer
;; chain after 40 iterations.
;;
;; However, since we are interested only in the counts, then a better
;; way to approach the problem presents itself. Rather than considering
;; the entire chain and thinking of individual characters, we instead
;; look at the pairs of characters and the number of times they appear.

;; First, we want to construct a list of every possible pair of
;; characters that could appear, which we can strip from the polymer
;; template and both sides of the pair insertion rules.
(defparameter unique-characters
  (remove-duplicates
   (str:split ""
	      (format nil
		      "~{~a~}"
		      (alexandria:flatten
		       (cons polymer-template
			     (mapcar (lambda (string) (str:split " -> " string))
				     pair-insertion-rules))))
	      :omit-nulls t)
   :test #'equalp))

(defparameter all-pairs
  (loop for char1 in unique-characters
	append
	(loop for char2 in unique-characters
	      collect (format nil "~{~a~}" (list char1 char2)))))
all-pairs
;;=> ("KK" "KC" "KO" "KF" "KV" "KS" "KN" "KH" "KP" "KB" "CK" "CC"
;;=>  "CO" "CF" "CV" "CS" "CN" "CH" "CP" "CB" "OK" "OC" "OO" "OF"
;;=>  "OV" "OS" "ON" "OH" "OP" "OB" "FK" "FC" "FO" "FF" "FV" "FS"
;;=>  "FN" "FH" "FP" "FB" "VK" "VC" "VO" "VF" "VV" "VS" "VN" "VH"
;;=>  "VP" "VB" "SK" "SC" "SO" "SF" "SV" "SS" "SN" "SH" "SP" "SB"
;;=>  "NK" "NC" "NO" "NF" "NV" "NS" "NN" "NH" "NP" "NB" "HK" "HC"
;;=>  "HO" "HF" "HV" "HS" "HN" "HH" "HP" "HB" "PK" "PC" "PO" "PF"
;;=>  "PV" "PS" "PN" "PH" "PP" "PB" "BK" "BC" "BO" "BF" "BV" "BS"
;;=>  "BN" "BH" "BP" "BB")

;; Now we simply keep track of each time that pair appears in
;; a polymer template. So to construct the initial polymer
;; template we can make use of our split-into-pairs function
;; from earlier:
(defparameter pair-counts
  (loop for pair in all-pairs collect (list pair 0)))

(loop for pair in (split-into-pairs polymer-template) do
  (incf (cadr (elt pair-counts
		   (position pair pair-counts :key #'car :test #'equalp)))))
pair-counts
;;=> (("KK" 0) ("KC" 1) ("KO" 0) ("KF" 0) ("KV" 0) ("KS" 0) ("KN" 0)
;;=>  ("KH" 0) ("KP" 0) ("KB" 0) ("CK" 0) ("CC" 1) ("CO" 0) ("CF" 0)
;;=>  ("CV" 1) ("CS" 1) ("CN" 0) ("CH" 0) ("CP" 0) ("CB" 0) ("OK" 0)
;;=>  ("OC" 0) ("OO" 1) ("OF" 0) ("OV" 0) ("OS" 0) ("ON" 0) ("OH" 0)
;;=>  ("OP" 1) ("OB" 2) ("FK" 0) ("FC" 0) ("FO" 0) ("FF" 0) ("FV" 0)
;;=>  ("FS" 0) ("FN" 0) ("FH" 0) ("FP" 1) ("FB" 0) ("VK" 0) ("VC" 0)
;;=>  ("VO" 0) ("VF" 0) ("VV" 0) ("VS" 0) ("VN" 0) ("VH" 1) ("VP" 0)
;;=>  ("VB" 0) ("SK" 0) ("SC" 0) ("SO" 1) ("SF" 0) ("SV" 0) ("SS" 0)
;;=>  ("SN" 0) ("SH" 0) ("SP" 0) ("SB" 0) ("NK" 0) ("NC" 0) ("NO" 1)
;;=>  ("NF" 0) ("NV" 0) ("NS" 0) ("NN" 0) ("NH" 0) ("NP" 0) ("NB" 0)
;;=>  ("HK" 1) ("HC" 0) ("HO" 1) ("HF" 0) ("HV" 0) ("HS" 0) ("HN" 0)
;;=>  ("HH" 0) ("HP" 0) ("HB" 0) ("PK" 0) ("PC" 0) ("PO" 0) ("PF" 0)
;;=>  ("PV" 0) ("PS" 0) ("PN" 1) ("PH" 0) ("PP" 0) ("PB" 1) ("BK" 0)
;;=>  ("BC" 1) ("BO" 0) ("BF" 1) ("BV" 0) ("BS" 0) ("BN" 0) ("BH" 1)
;;=>  ("BP" 0) ("BB" 0))

;; In this new representation, see that each of our pairwise
;; insertion rules now becomes a replacement rule: the LHS pair
;; is removed, and replaced with two new pairs defined by the
;; rule. We can combine our old functions to do this too, and
;; code up a function that performs a single step of the pair
;; 'insertion' process like so:
(defun insertion-step ()
  "Run a single step of the pair insertion process."
  (let ((initial-counts (copy-tree pair-counts)))
    (loop for (pair count) in initial-counts do
      (if (> count 0)
	  (let ((new-pairs (split-into-pairs (pair-insertion pair))))
	    (if new-pairs
		(progn
		  (setf (cadr (elt pair-counts
				   (position pair
					     pair-counts
					     :key #'car
					     :test #'equalp)))
			(- (cadr (elt pair-counts
				      (position pair
						pair-counts
						:key #'car
						:test #'equalp)))
			   count))
		  (loop for new-pair in new-pairs do
		    (setf (cadr (elt pair-counts
				     (position new-pair
					       pair-counts
					       :key #'car
					       :test #'equalp)))
			  (+ (cadr (elt pair-counts
					(position new-pair
						  pair-counts
						  :key #'car
						  :test #'equalp)))
			     count))))))))))

;; So we can perform 10 iterations to make sure we get the
;; same answer again as in Part 1:
(loop repeat 10 do (insertion-step))

;; Now to count up the characters, we simply add up how
;; many times a character appears as either the left-most or
;; right-most character in a pair. Now recall that our pairs
;; have redundency between them, so if we count every single
;; occurrence we'll inevitably double-count all of the characters
;; except for the very first one at the head of the polymer chain
;; and the very last one at the end of the polymer. But since these
;; never change anyway, we can just take them into account after we
;; halve everything to get an accurate count of the characters.
;;
;; So our new character-counting function is
(defun count-characters ()
  "Return a list of all of the character counts (in the pair counts)."
  (let ((char-counts (loop for char in unique-characters
			   collect (list char 0)))
	(first-char (str:substring 0
				   1
				   polymer-template))
	(last-char (str:substring (1- (length polymer-template))
				  (length polymer-template)
				  polymer-template)))
    (loop for (pair count) in pair-counts do
      (loop for char in (str:split "" pair :omit-nulls t) do
	(setf (cadr (elt char-counts
			 (position char char-counts :key #'car :test #'equalp)))
	      (+ (cadr
		  (elt char-counts 
		       (position char char-counts :key #'car :test #'equalp)))
		 count))))
    ;; Double-counted, so divide by 2 and add back 1/2 each for
    ;; the first and last characters.
    ;; (TODO: This list cadr access pattern really should be a macro...)
    (loop for (char count) in char-counts do
      (setf (cadr (elt char-counts
		       (position char char-counts :key #'car :test #'equalp)))
	    (/ (cadr
		(elt char-counts
		     (position char char-counts :key #'car :test #'equalp)))
	       2)))
    (loop for char in (list first-char last-char) do
      (setf (cadr (elt char-counts
		       (position char char-counts :key #'car :test #'equalp)))
	    (+ (cadr
		(elt char-counts
		     (position char char-counts :key #'car :test #'equalp)))
	       1/2)))
    char-counts))

(count-characters)
;;=> (("K" 3077) ("C" 2285) ("O" 921) ("F" 3115) ("V" 1485)
;;=>  ("S" 2339) ("N" 1215) ("H" 1966) ("P" 1154) ("B" 1900))

;; This matches perfectly with the answer we got for 10 iterations
;; earlier, so now we can confidently and effortlessly compute
;; the remaining 30 iterations to find the new character counts.
(loop repeat 30 do (insertion-step))

(defparameter char-counts (count-characters))
char-counts
;;=> (("K" 3403627694467) ("C" 2293130898400) ("O" 1043328798690)
;;=>  ("F" 3198325744551) ("V" 1686053944672) ("S" 2473826999765)
;;=>  ("N" 1197538113113) ("H" 2216958989059) ("P" 1314805424261)
;;=>  ("B" 2063124320767))

(most-common char-counts)
;;=> ("K" 3403627694467)

(least-common char-counts)
;;=> ("O" 1043328798690)

(- (cadr (most-common char-counts)) (cadr (least-common char-counts)))
;;=> 2360298895777
