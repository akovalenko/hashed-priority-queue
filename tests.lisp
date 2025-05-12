(defpackage #:hashed-priority-queue/test
  (:use :cl :fiveam :hashed-priority-queue :alexandria))

(in-package #:hashed-priority-queue/test)
(def-suite :hashed-priority-queue)
(in-suite :hashed-priority-queue)

(defun q-of (&rest args)
  (let* ((test (if (stringp (first args)) 'equal 'eql))
	 (q (make-hpqueue :test test))
	 (alist (loop for index from 0
		      for arg in args
		      collect (cons index arg))))
    (setf alist (alexandria:shuffle alist))
    (loop for (prio . elt) in alist
	  do (hpqueue-push elt prio q))
    q))

(defun det-q-of (&rest args)
  (let* ((test (if (stringp (first args)) 'equal 'eql))
	 (q (make-hpqueue :test test))
	 (alist (loop for index from 0
		      for arg in args
		      collect (cons index arg))))
    (loop for (prio . elt) in alist
	  do (hpqueue-push elt prio q))
    q))

(test sorting
  (let ((q (make-hpqueue :test 'equal)))
    (is (eql 120 (hpqueue-push "u" 120 q)))
    (hpqueue-push "a" 50 q)
    (hpqueue-push "b" 40 q)
    (hpqueue-push "b" 55 q)
    (is (eql 56 (hpqueue-push "b" 1 q '+)))
    (is (equal '("a" "b" "u")
	       (loop for popped = (hpqueue-pop q)
		     while popped collect popped)))))

(test predicate
  (let ((q (make-hpqueue :test 'equal :predicate '>)))
    (hpqueue-push "u" 120 q)
    (hpqueue-push "a" 50 q)
    (hpqueue-push "b" 55 q)
    (is (equal '("u" "b" "a")
	       (loop for popped = (hpqueue-pop q)
		     while popped collect popped)))))

(test multiple-values
  (is (endp (multiple-value-list (hpqueue-pop (make-hpqueue)))))
  (let ((q (make-hpqueue)))
    (hpqueue-push 100 500 q)
    (is (equal '(100 500 t) (multiple-value-list (hpqueue-pop q))))))

(test copy-hpqueue
  (let* ((q (q-of "alpha" "bravo" "charlie"))
	 (copy (copy-hpqueue q)))
    (loop until (hpqueue-empty q)
	  do (is (eq (hpqueue-pop q)
		     (hpqueue-pop copy))))
    (is (endp (hpqueue-pop copy)))
    (setf q (q-of "alpha" "bravo" "charlie")
	  copy (copy-hpqueue q))
    (clear-hpqueue copy)
    (is (equal "alpha" (hpqueue-pop q)))
    (is (endp (hpqueue-pop copy)))))

(test map-hpqueue
  (let ((list (list "alpha" "bravo" "charlie"))
	(collected))
    (map-hpqueue (lambda (element prio)
		   (declare (ignorable prio))
		   (push element collected))
		 (apply 'q-of list))
    (is (equal (sort collected #'string<) (sort list #'string<)))))

(test hpqueue-priority
  (let ((q (q-of "a" "b" "c")))
    (is (eql 1 (hpqueue-priority "b" q)))
    (is (eql 2 (hpqueue-priority "c" q)))
    (is (eql 100500 (hpqueue-priority "jarjar" q 100500)))
    (setf (hpqueue-priority "c" q) -10)
    (is (equal "c" (hpqueue-pop q)))))

(test hpqueue-reduce-priority
  (let ((q (make-hpqueue)))
    (is (eql 10 (hpqueue-push :a 10 q '+)))
    (is (eql 20 (hpqueue-push :a 10 q '+)))
    (is (eql 15 (hpqueue-push :b 15 q '+)))
    (is (eq :b (hpqueue-pop q)))
    (is (eq :a (hpqueue-pop q)))
    (is (null (hpqueue-pop q)))))

(test hash-table-hpqueue
  (let ((q (hash-table-hpqueue
	    (alist-hash-table '(("foo" . 200) ("bar" . 10) ("baz" . 50))
			      :test 'equal))))
    (is (equal "bar" (hpqueue-pop q)))
    (is (equal "baz" (hpqueue-pop q)))
    (is (equal "foo" (hpqueue-pop q)))
    (is (null (hpqueue-pop q))))
  (let* ((r (loop repeat 1000
		  collect (random 1000000)))
	 (q (hash-table-hpqueue
	     (alist-hash-table
	      (loop for item in r
		    collect (cons (list t) item))))))
    (is (equalp (sort r #'<)
		(loop for item = (nth-value 1 (hpqueue-pop q))
		      while item
		      collect item)))))

(test hpqueue-front
  (is (equal '("a" 0 t)
	     (multiple-value-list
	      (hpqueue-front (q-of "a" "b")))))
  (is (null (multiple-value-list (hpqueue-front (q-of))))))

(test hpqueue-empty
  (is (hpqueue-empty (q-of)))
  (is (not (hpqueue-empty (q-of 123)))))

(test hpqueue-count
  (is (= 7 (hpqueue-count (q-of 1 2 3 4 5 6 7))))
  (is (= 0 (hpqueue-count (q-of)))))

(test hpqueue-delete
  (let* ((q1 (q-of "a" "b" "c"))
	 (q2 (q-of "a" "b")))
    (hpqueue-delete "c" q1)
    (hpqueue-delete "missing" q1)
    (is (hpqueue-equal q1 q2))))

(test hpqueue-delete-pos-smashing
  (let ((q (det-q-of :a :b :c)))
    (hpqueue-delete :b q)
    (setf (hpqueue-priority :c q) -1)
    (is (eql 0 (cdr (assoc :a (hpqueue-alist q)))))))

(test clear-hpqueue
  (let ((q (q-of "a")))
    (is (hpqueue-empty (clear-hpqueue q)))))

(test hpqueue-hash-table
  (is (equal (sort
	      (hash-table-alist
	       (hpqueue-hash-table (q-of "a" "b" "c")))
	      #'< :key 'cdr)
	     '(("a" . 0) ("b" . 1) ("c" . 2)))))

(test hpqueue-alist
  (is (equal (sort
	      (hpqueue-alist (q-of "a" "b" "c"))
	      #'< :key 'cdr)
	     '(("a" . 0) ("b" . 1) ("c" . 2)))))

(test hpqueue-equal
  (is (hpqueue-equal (q-of 1 2 4)
		     (q-of 1 2 4)))
  (is (not (hpqueue-equal (q-of 1 2)
			  (q-of 1 2 4))))
  (is (not (hpqueue-equal (q-of 1 2 4)
			  (q-of 1 2)))))

(test merge-hpqueue
  (is (hpqueue-equal
       (merge-hpqueue '+ (q-of 1 2 5) (q-of 15 38 1))
       (merge-hpqueue '+ (q-of 15 38 1) (q-of 1 2 5))))

  (is (eql 4 (hpqueue-priority "c"
			       (merge-hpqueue '*
					      (q-of "a" "b" "c")
					      (q-of "x" "a" "c"))))))

;;; generic dijkstra pathfinding
(defun dijkstra-find-path (initial get-neighbor-alist-fn
			   target &key (test 'eql))
  (let ((queue (make-hpqueue :test test))
	(visited (make-hash-table :test test)))
    (hpqueue-push initial 0 queue)
    (loop
      (multiple-value-bind (item cost present) (hpqueue-pop queue)
	(unless present (return (values nil visited)))
	(setf (gethash item visited) cost)
	(when (funcall test item target)
	  (return (values cost visited)))
	(loop for (neighbor . distance)
		in (funcall get-neighbor-alist-fn item)
	      do (unless (gethash neighbor visited)
		   (hpqueue-push
		    neighbor (+ cost distance) queue #'min)))))))

;;; applying dijkstra-find-path to a data set taken from
;;; https://adventofcode.com/2021/day/15

(defun map-neighbors (function location dimensions)
  (destructuring-bind (w &rest ws) dimensions
    (destructuring-bind (x &rest xs) location
      (when (< -1 (1- x) w)
	(funcall function (cons (1- x) xs)))
      (when (< -1 (1+ x) w)
	(funcall function (cons (1+ x) xs)))
      (when xs
	(flet ((has-tail (tail)
		 (funcall function (cons x tail))))
	  (map-neighbors #'has-tail xs ws))))))

(defun list-neigbors (location dimensions)
  (let (list)
    (map-neighbors
     (lambda (loc)
       (push loc list))
     location dimensions)
    list))

(defparameter *field*
  (with-input-from-string (input "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")
    (coerce (loop for line = (read-line input nil)
		  while line
		  collect line)
	    'vector)))

(defun cost-at (x y)
  (1+ (mod (+
	    (1- (digit-char-p
		 (aref (aref *field* (mod y 10))
		       (mod x 10))))
	    (floor x 10)
	    (floor y 10))
	   9)))

(test dijkstra-find-path
  (is (= 315
	 (let* ((start '(0 0))
		(dimensions '(50 50))
		(target (mapcar '1- dimensions)))
	   (flet ((neighbors (cell)
		    (loop for (x y) in (list-neigbors cell dimensions)
			  collect (cons (list x y) (cost-at x y)))))
	     (dijkstra-find-path start #'neighbors target :test 'equal))))))


;;; Running median

;;; Problem: you have a stream of incoming numbers. For each new
;;; number after the (M-1)th, calculate the median of last M numbers
;;; received.
;;;
;;; Solution: keep a max-heap (left) and a min-heap (right),
;;; maintaining that:
;;;
;;; - size difference between heaps is at most 1 (rebalance heaps when
;;; the difference becomes larger)
;;;
;;; - max(left) <= min(right)
;;;
;;; The median is the root of the longest heap if their sizes are
;;; different, and the average of the heap roots otherwise.
;;;
;;; Old elements are removed from the heaps as they fall out of the
;;; running window. To achieve this, an identifier is attached to
;;; elements, cycling from 0 to M-1 (mod M).

(defun avg (n1 &rest ns)
  (loop with length = (1+ (length ns))
	for n in (list* n1 ns)
	sum (/ n length)))

(defstruct (rm (:constructor %make-rm))
  left right
  (size nil :type (mod #.array-dimension-limit))
  (pred< nil :type function)
  (pred> nil :type function)
  (tie nil :type function)
  (next-id nil :type (mod #.array-dimension-limit)))

(defun fun (designator)
  (etypecase designator
    (symbol (symbol-function designator))
    (function designator)))

(defun make-rm (size &key (predicate #'<) (tie-breaker #'avg))
  (setf predicate (fun predicate))
  (setf tie-breaker (fun tie-breaker))
  (flet ((pred< (a b)
	   (funcall predicate a b))
	 (pred> (a b)
	   (funcall predicate b a)))
    (%make-rm :size size
	      :next-id 0
	      :tie tie-breaker
	      :pred< #'pred<
	      :pred> #'pred>
	      :left (make-hpqueue :predicate #'pred>)
	      :right (make-hpqueue :predicate #'pred<))))

(defun under-root-p (prio queue predicate)
  (multiple-value-bind (elt top-prio present) (hpqueue-front queue)
    (declare (ignore elt))
    (and present (funcall predicate top-prio prio))))

(defun rm-get-id (rm)
  (with-accessors ((next-id rm-next-id)
		   (size rm-size))
      rm
    (prog1 (mod next-id size)
      (setf next-id (mod (1+ next-id) size)))))

(defun push-rm (value rm)
  (let ((left (rm-left rm))
	(right (rm-right rm)))
    (let ((id (rm-get-id rm)))
      ;; remove stale element
      (or (nth-value 1 (hpqueue-delete id left))
	  (hpqueue-delete id right))

      (flet ((best-queue ()
	       (cond
		 ((under-root-p value left (rm-pred> rm)) left)
		 ((under-root-p value right (rm-pred< rm)) right)
		 (t (if (< (hpqueue-count left)
			   (hpqueue-count right))
			left right)))))
	(hpqueue-push id value (best-queue))
	(loop while (< 1 (- (hpqueue-count left) (hpqueue-count right)))
	      ;; while left is more than 1 bigger than right
	      do (multiple-value-bind (elt prio) (hpqueue-pop left)
		   (hpqueue-push elt prio right)))
	(loop while (< 1 (- (hpqueue-count right) (hpqueue-count left)))
	      do (multiple-value-bind (elt prio) (hpqueue-pop right)
		   (hpqueue-push elt prio left)))))))

(defun rm-median (rm)
  (let* ((left (rm-left rm))
	 (right (rm-right rm))
	 (left-size (hpqueue-count left))
	 (right-size (hpqueue-count right)))

    (when (= 0 left-size right-size)
      (return-from rm-median))

    (if (= left-size right-size)
	(funcall (rm-tie rm)
		 (nth-value 1 (hpqueue-front left))
		 (nth-value 1 (hpqueue-front right)))
	(if (< left-size right-size)
	    (nth-value 1 (hpqueue-front right))
	    (nth-value 1 (hpqueue-front left))))))

(test running-median
  (let ((vector (make-array 1000))
	(rm (make-rm 17)))
    (map-into vector (lambda () (random 400000)))
    (loop for item across vector
	  do (push-rm item rm))
    (let ((last-chunk (subseq vector (- 1000 17))))
      (is (eql (elt (sort last-chunk #'<) 8)
	       (rm-median rm))))))
