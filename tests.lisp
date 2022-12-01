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
	(when (funcall test item target)
	  (return (values cost visited)))
	(setf (gethash item visited) cost)
	(loop for (neighbor . distance)
		in (funcall get-neighbor-alist-fn item)
	      do (hpqueue-push
		  neighbor (+ cost distance) queue #'min))))))

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
