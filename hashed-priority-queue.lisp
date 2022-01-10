;;;; hashed-priority-queue.lisp

(in-package #:hashed-priority-queue)

;;;; initial array size
(defconstant +default-size+ 16)

(declaim (inline hpqueue-array
		 hpqueue-hash-table
		 hpqueue-predicate
		 hpqueue-test))

;;;; hashed priority queue
(defstruct (hpqueue (:constructor %make-hpqueue)
		    (:copier %copy-hpqueue))
  (array (make-array +default-size+ :adjustable t :fill-pointer 0)
   :type (vector t *)
   :read-only t)
  (%hash-table nil
   :type hash-table
   :read-only t)
  (predicate #'<
   :type function
   :read-only t))

(defun hpqueue-test (queue)
  (hash-table-test (hpqueue-%hash-table queue)))

(declaim (inline node-pos
		 node-prio
		 node-element))

(defstruct node
  (pos 0 :type (mod #.array-dimension-limit))
  prio element)

(defun %parent (pos)
  (and (plusp pos) (floor (1- pos) 2)))

(defun %sift-up (queue pos)
  (loop
    with hpqueue-array = (hpqueue-array queue)
    with predicate = (hpqueue-predicate queue)
    with node-prio = (node-prio (aref hpqueue-array pos))
    for parent = (%parent pos)
    while parent
    while (funcall predicate node-prio (node-prio (aref hpqueue-array parent)))
    do (rotatef (node-pos (aref hpqueue-array parent))
		(node-pos (aref hpqueue-array pos)))
       (rotatef (aref hpqueue-array parent)
		(aref hpqueue-array pos))
       (setf pos parent)))

(defun %sift-down (queue pos)
  (loop
    with hpqueue-array = (hpqueue-array queue)
    with predicate = (hpqueue-predicate queue)
    with length = (length hpqueue-array)
    with node-prio = (node-prio (aref hpqueue-array pos))
    for n1 = (1+ (* pos 2))
    for n2 = (1+ n1)
    while (< n1 length) ;; there's a "down" to sift
    do (let ((n-best (if (or (= n2 length)
			     (funcall predicate
				      (node-prio (aref hpqueue-array n1))
				      (node-prio (aref hpqueue-array n2))))
			 n1 n2)))
	 (unless (funcall predicate
			  (node-prio (aref hpqueue-array n-best))
			  node-prio)
	   (return))
	 (rotatef (node-pos (aref hpqueue-array pos))
		  (node-pos (aref hpqueue-array n-best)))
	 (rotatef (aref hpqueue-array pos)
		  (aref hpqueue-array n-best))
	 (setf pos n-best))))

(defun %heapify-range (queue start end)
  (loop for i from start below end
	do (%sift-down queue i)))

(defun %heapify (queue)
  (let* ((array (hpqueue-array queue))
	 (length (length array)))
    (loop
      (let ((start (or (%parent (1- length))
		       (return queue))))
	(%heapify-range queue start (1+ (* 2 start)))
	(setf length (1+ start))))))

(defun make-hpqueue (&key (test 'eql) (predicate '<))
  "Make a hashed priority queue, setting element hash table test to TEST
and priority comparison predicate to PREDICATE. Default predicate
value of '< gives you a min-heap."
  (flet ((fun (arg)
	   (etypecase arg
	     (symbol (symbol-function arg))
	     (function arg))))
    (%make-hpqueue :%hash-table (make-hash-table :test (fun test))
		   :predicate (fun predicate))))

(defun copy-hpqueue (queue)
  "Make a shallow copy of a hashed priority queue, duplicating its
internal data structures and filling it with the same (EQL) elements
and priorities."
  (let* ((hash-table (hpqueue-%hash-table queue))
	 (array (hpqueue-array queue))
	 (new (%make-hpqueue :predicate (hpqueue-predicate queue)
			     :%hash-table (make-hash-table
					   :test (hash-table-test hash-table)
					   :size (hash-table-size hash-table))
			     :array (make-array (length array)
						:adjustable t
						:fill-pointer t))))
    (loop
      with new-hash-table = (hpqueue-%hash-table new)
      for node across (map-into (hpqueue-array new) #'copy-node array)
      do (setf (gethash (node-element node) new-hash-table) node))
    new))

(defun map-hpqueue (function queue)
  "Call FUNCTION for each element of QUEUE with two arguments: the
element and its priority."
  (loop for node across (hpqueue-array queue)
	do (funcall function (node-element node) (node-prio node))))

(defun clear-hpqueue (queue)
  "Make QUEUE empty. Return QUEUE."
  (map-into (hpqueue-array queue) (constantly nil))
  (setf (fill-pointer (hpqueue-array queue)) 0)
  (clrhash (hpqueue-%hash-table queue))
  queue)

(defun hash-table-hpqueue (hash-table &optional (predicate #'<))
  "Make a hashed priority queue and populate it from HASH-TABLE, using
its keys as elements and its values as priorities. The TEST property
of a queue will be HASH-TABLE's test."
  (let* ((new-hash-table
	   (make-hash-table
	    :test (hash-table-test hash-table)
	    :size (hash-table-size hash-table)))
	 (#1=predicate (etypecase #1# (function #1#) (symbol (symbol-function #1#))))
	 (array (make-array (hash-table-count hash-table)
			    :adjustable t :fill-pointer t))
	 (queue (%make-hpqueue :predicate predicate
			       :array array
			       :%hash-table new-hash-table)))
    (loop for k being the hash-key of hash-table using (hash-value v)
	  for pos from 0
	  do (setf (gethash k new-hash-table)
		   (setf (aref array pos)
			 (make-node :pos pos :prio v :element k))))
    (%heapify queue)))

(defun hpqueue-hash-table (queue)
  (let* ((%hash-table (hpqueue-%hash-table queue))
	 (hash-table (make-hash-table :test (hash-table-test %hash-table)
				      :size (hash-table-size %hash-table))))
    (maphash (lambda (k v) (setf (gethash k hash-table) (node-prio v)))
	     %hash-table)
    hash-table))

(defun hpqueue-alist (queue)
  (loop for node across (hpqueue-array queue)
	collect (cons (node-element node)
		      (node-prio node))))

(defun merge-hpqueue (join-function queue &rest queues)
  "Merge QUEUE with other QUEUES, applying JOIN-FUNCTION to get a new
priority for any element present in several queues at once. Test and
predicate are takend from the first QUEUE.

JOIN-FUNCTION is called with two arguments: the established priority
of an element and the new observed priority, and is expected to return
the new established priority. Queues are examined from left to right.

Merging large enough queues is more efficient than pushing elements
one-by-one."
  (unless queues
    (return-from merge-hpqueue (copy-hpqueue queue)))
  (let ((hash-table (hpqueue-hash-table queue)))
    (loop for item in queues
	  do (map-hpqueue (lambda (element priority)
			    (multiple-value-bind (old-prio present-p)
				(gethash element hash-table)
			      (setf (gethash element hash-table)
				    (if present-p
					(funcall join-function old-prio priority)
					priority))))
			  item))
    (hash-table-hpqueue hash-table (hpqueue-predicate queue))))

(defun hpqueue-push (element priority queue &optional join-function)
  "Insert ELEMENT into the QUEUE with PRIORITY, or update its priority
if it's already there. If JOIN-FUNCTION is given, it's expected to
return a new priority for an existing element when called with two
arguments: element's original priority and the value of PRIORITY. If
JOIN-FUNCTION is NIL, the new priority just replaces the old.

Return the new priority value assigned to the ELEMENT."
  (flet ((override (old new) (declare (ignorable old)) new))
    (let* ((node (gethash element (hpqueue-%hash-table queue)))
	   (old-prio (and node (node-prio node)))
	   (new-prio (if node
			 (funcall (or join-function #'override)
				  old-prio priority)
			 priority))
	   (predicate (hpqueue-predicate queue)))
      (when (and node (eql new-prio old-prio))
	(return-from hpqueue-push new-prio))
      (let ((up (or (not node) (funcall predicate new-prio old-prio)))
	    (hpqueue-array (hpqueue-array queue)))
	(if node
	    (setf (node-prio node) new-prio)
	    (setf node (make-node :prio new-prio :element element)
		  (node-pos node) (vector-push-extend node hpqueue-array)
		  (gethash element (hpqueue-%hash-table queue))
		  node))
	(if up
	    (%sift-up queue (node-pos node))
	    (%sift-down queue (node-pos node)))
	new-prio))))

(defun hpqueue-pushnew (element priority queue)
  "Insert ELEMENT to a QUEUE with PRIORITY if it's not there yet and return T.
If the element is already there, do nothing return NIL."
  (hpqueue-push element priority queue
		(lambda (new old)
		  (declare (ignore new old))
		  (return-from hpqueue-pushnew)))
  t)

(defun hpqueue-priority (element queue &optional default)
  "Find the ELEMENT in the QUEUE and return its priority. If there's no
such element, return DEFAULT.

Primary return value is a priority or DEFAULT, secondary is T iff the
element is really present, NIL if default was used."
  (let ((node (gethash element (hpqueue-%hash-table queue))))
    (if node
	(values (node-prio node) t)
	(values default nil))))

(defun (setf hpqueue-priority) (priority element queue &optional ignored)
  (declare (ignorable ignored))
  (hpqueue-push element priority queue))

(defun hpqueue-count (queue)
  "Return number of elements in QUEUE."
  (length (hpqueue-array queue)))

(defun hpqueue-empty (queue)
  "Return true iff the QUEUE is empty"
  (zerop (hpqueue-count queue)))

(defun hpqueue-pop (queue)
  "Extract the top element from a hashed priority QUEUE.

Return values: element, priority, T upon successful extraction, no
values if the queue is empty."
  (let ((vector (hpqueue-array queue)))
    (when (zerop (length vector))
      (return-from hpqueue-pop (values)))
    (let* ((head (aref vector 0))
	   (tail (vector-pop vector)))
      (multiple-value-prog1
	  (values (node-element head) (node-prio head) t)
	(remhash (node-element head) (hpqueue-%hash-table queue))
	(setf (node-pos tail) 0
	      (aref vector 0) tail)
	(%sift-down queue 0)))))

(defun hpqueue-front (queue)
  "Peek the top element from a hashed priority QUEUE. Predicate '< (the
default for queue creation) makes the minimal element the top.

Return values: element, priority, T upon successful extraction, no
values if the queue is empty."
  (let ((array (hpqueue-array queue)))
    (if (plusp (length array))
	(let ((node (aref array 0)))
	  (values (node-element node)
		  (node-prio node)
		  t))
	(values))))

(defun hpqueue-equal (q1 q2)
  "Return true iff q1 and q2 are both hpqueues, their tests and
predicates are EQL, and they have the same elements (under their
tests) with the same priorities.

Priorities are compared using the predicate, which is assumed to be a
strict comparison on a fully-ordered set: two priorities are equal
when neither is strictly better than the other. E.g. when a predicate
is '< (which is the default), 5.0 and 5 are considered equal
priorities."
  (flet ((ensure (bool) (unless bool (return-from hpqueue-equal))))
    (ensure (hpqueue-p q1))
    (ensure (hpqueue-p q2))
    (ensure (eql (hpqueue-test q1) (hpqueue-test q2)))
    (let ((p1 (hpqueue-predicate q1))
	  (p2 (hpqueue-predicate q2)))
      (ensure (eql p1 p2))
      (labels ((prio-eql (prio1 prio2)
		 (and (not (funcall p1 prio1 prio2))
		      (not (funcall p1 prio2 prio1))))
	       (compare (mapped checked)
		 (map-hpqueue
		  (lambda (element priority)
		    (multiple-value-bind (other-prio present-p)
			(hpqueue-priority element checked)
		      (ensure present-p)
		      (ensure (prio-eql priority other-prio))))
		  mapped)))
	(compare q1 q2)
	(compare q2 q1)
	t))))
