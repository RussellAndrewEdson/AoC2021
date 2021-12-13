;;; Code for Advent of Code 2021, Day 12.
;;;
;;; Code author: Russell A. Edson
;;; Date last modified: 13/12/2021

;;; Problem 1
;; Here we are given as input the edges in a graph connecting caves
;; to each other, and we need to determine all possible paths through
;; the graph from start to end.
(ql:quickload '(:uiop :str :alexandria))
(defparameter edges
  (mapcar (lambda (string) (str:split "-" string))
	  (uiop:read-file-lines "day12_problem1.txt")))

;; Now we want to code some utility functions that return us the
;; unique nodes given this list of edges, as well as the nodes
;; that are connected to a given node so that we can traverse
;; the graph. We also want a function that prunes a given node
;; from the edge set. We can code these as follows:
(defun nodes (edges)
  "Return the list of unique nodes for the graph given the EDGES set."
  (remove-duplicates (alexandria:flatten edges) :test #'equalp))

(nodes edges)
;;=> ("XG" "xm" "TS" "end" "ci" "gy" "YW" "gz" "DK" "zq" "la" "start" "TF")

(defun connected-nodes (node edges)
  "Return the connected nodes for this NODE given the EDGES set."
  (remove-if
   (lambda (other-node) (equalp node other-node))
   (alexandria:flatten
    (remove-if-not
     (lambda (edge) (or (equalp node (car edge))
			(equalp node (cadr edge))))
     edges))))

(connected-nodes "start" edges)
;;=> ("ci" "DK" "TF")

(defun prune-node (node edges)
  "Return the new edge set after pruning NODE from the EDGES graph."
  (remove-if
   (lambda (edge) (or (equalp node (car edge))
		      (equalp node (cadr edge))))
   edges))

(prune-node "la" edges)
;;=> (("YW" "end") ("end" "gy") ("zq" "ci") ("XG" "gz") ("gy" "gz")
;;=>  ("ci" "start") ("YW" "ci") ("TF" "zq") ("ci" "DK") ("zq" "YW")
;;=>  ("gz" "YW") ("zq" "gz") ("end" "gz") ("ci" "TF") ("DK" "zq")
;;=>  ("gy" "YW") ("start" "DK") ("gz" "DK") ("start" "TF"))

(defun big-cave? (node)
  "True if this node is a big cave that can be visited more than once."
  (str:upcase? node))

;; We can then work on the set of edges as our data representation
;; of the graph directly, and code a function to (recursively) traverse
;; the graph to find all possible paths through the caves.
(defun traverse (edges &optional (start "start") (end "end"))
  "Return all paths through the graph EDGES from START to END."
  (cond ((equalp start end) (list (list end)))
	(t (loop for node in (connected-nodes start edges)
		 append
		 (let ((edges (if (big-cave? start)
				  edges
				  (prune-node start edges))))
		   (mapcar (lambda (other-edges)
			     (append (list start) other-edges))
			   (traverse edges node end)))))))

;; Finally, we can collect the paths and count them.
(defparameter paths (traverse edges))
(length paths)
;;=> 4912


;;; Problem 2
;; Now we are given that we can visit a single small cave exactly twice
;; (except for start and end). So to fix our code we want to keep
;; track of the number of times we've visited each cave during
;; the traversal.
(defun max-visits (node)
  "Return the maximum number of times NODE can be visited (-1=INF)."
  (cond ((equalp node "start") 1)
	((equalp node "end") 1)
	((str:downcase? node) 2)
	(t -1)))

(defun traverse-part2 (edges)
  "Return all paths through EDGES, but we can visit a small cave twice."
  (let ((initial-visits (mapcar #'list
				(nodes edges)
				(mapcar #'max-visits (nodes edges)))))
    (labels ((traverse-iter
		 (edges &optional
			  (start "start")
			  (end "end")
			  (visits initial-visits)
			  (visited-small-cave-twice? nil))
	       (let ((visits-left (copy-tree visits)))
		 (labels ((visit (node)
			    (decf (cadr (elt visits-left
					     (position node
						       visits-left
						       :key #'car
						       :test #'equalp)))))
			  (can-visit? (node)
			    (/= 0 (cadr (elt visits-left
					     (position node
						       visits-left
						       :key #'car
						       :test #'equalp))))))
		   (visit start)
		   ;; If we've visited a small cave twice, then all of the
		   ;; other small caves cannot be visited twice anymore.
		   (let ((small-caves (remove-if
				       (lambda (node)
					 (or (equalp (car node) "start")
					     (equalp (car node) "end")
					     (big-cave? (car node))))
				       visits-left)))
		     (if (and (zerop (apply #'min (mapcar #'cadr small-caves)))
			      (null visited-small-cave-twice?))
			 (progn
			   (mapcar
			    (lambda (node)
			      (setf (cadr (elt visits-left
					       (position node
							 visits-left
							 :key #'car
							 :test #'equalp)))
				    (max 0
					 (1-
					  (cadr
					   (elt visits-left
						(position node
							  visits-left
							  :key #'car
							  :test #'equalp)))))))
			    (mapcar #'car small-caves))
			   (setf visited-small-cave-twice? t))))
		   (cond ((equalp start end) (list (list end)))
			 (t (loop for node in (remove-if-not #'can-visit?
							     (connected-nodes
							      start
							      edges))
				  append
				  (let ((edges (if (can-visit? start)
						   edges
						   (prune-node start edges))))
				    (mapcar (lambda (other-edges)
					      (append (list start) other-edges))
					    (traverse-iter
					     edges
					     node
					     end
					     visits-left
					     visited-small-cave-twice?))))))))))
      (traverse-iter edges))))
    
;; Now the number of paths through the cave system is:
(defparameter paths-part2 (traverse-part2 edges))
(length paths-part2)
;;=> 150004
