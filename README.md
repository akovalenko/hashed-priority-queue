# hashed-priority-queue

### _Anton Kovalenko <anton@sw4me.com>_

This is a library providing heap-based priority queues, with an
additional feature of keeping track which element is where,
facilitating fast adjustment of existing element priorities.

## Interface by Example

```common-lisp
(let ((queue (make-hpqueue :predicate '< :test 'equalp)))
  ;; :predicate '< makes it a min-heap, which is the default.
  ;; :test 'equalp for case-insensitive string lookup

  (assert (hpqueue-p queue))
  (setf (hpqueue-priority "leader" queue) 1200
	    (hpqueue-priority "loser" queue) 1400)

  ;; add new element or update existing element's priority 
  (hpqueue-push "intermediate" 1300 queue)

  ;; read a priority of existing element
  (print (hpqueue-priority "LOSER" queue))

  ;; read with a default
  (print (hpqueue-priority "genie" queue :not-found))

  ;; either insert an element with a given priority
  ;; or apply a function to the old priority and
  ;; the given priority to get the new one. Commutative ops
  ;; which are also identities when applied to one argument,
  ;; like '+ and 'min usually make much sense here.

  (hpqueue-push "intermediate" 1700 queue 'min)
  (hpqueue-push "intermediate" -50 queue '+)

  ;; the magic of CL places, applied to element priorities
  ;; HPQUEUE-REDUCE-PRIORITY above is normally a bit more
  ;; effective and sometimes more readable
  (incf (hpqueue-priority "user" queue 0) 1550)

  ;; efficient shallow copy
  (setf queue (copy-hpqueue queue))

  ;; iteration
  (map-hpqueue (lambda (element priority)
                 (print (list element priority))) queue)

  ;; extract the front element
  (print (multiple-value-list (hpqueue-pop queue)))

  ;; peek the front element without extraction
  (print (multiple-value-list (hpqueue-front queue)))

  ;; clear a queue
  (clear-hpqueue queue)

  ;; convert to alist
  (assert (endp (hpqueue-alist queue)))

  ;; convert to hash table
  (assert (zerop (hash-table-count (hpqueue-hash-table queue))))

  ;; check if empty
  (assert (hpqueue-empty queue)))
```

## Functions

### `(MAKE-HPQUEUE &key PREDICATE TEST)`

Make a new, empty hashed priority queue with given properties
(PREDICATE and TEST) specified with keyword arguments (both are
function designators).

`PREDICATE` is a two-argument comparison function, whose generalized
boolean result indicates whether the priority value given by the first
argument is ‘better’ than the second. Default of `'<` gives you a
min-heap with numeric priorities.

`TEST` is an equality test suitable for `MAKE-HASH-TABLE`. The notion
of existing vs fresh elements in the queue depends on it.

### `(HPQUEUE-PRIORITY element queue &optional default)`

This is a SETFable place, returning two values when read: a priority
(falling back to DEFAULT when it's given) and an indicator of element
presence (T when it's a real priority, NIL for defaulting).

### `(HPQUEUE-PUSH element priority queue &optional join-function)`

If an element is not in the queue yet, add it with PRIORITY. Otherwise
adjust existing element priority to a return value of JOIN-FUNCTION
called with two arguments: the old priority and the new PRIORITY argument.

If JOIN-FUNCTION is nil, the new value of PRIORITY is unconditionally
assigned to an existing ELEMENT. Functions like 'MIN and '+, which are
commutative and are identities when called with one argument,
sometimes make sense as JOIN-FUNCTION in real tasks.

Return new element priority.

### `(HPQUEUE-PUSHNEW element priority queue)`

Add an element to the queue with priority if it's not there yet.
Return true if the element was added, NIL otherwise.

### `(HPQUEUE-POP queue)`

Remove the element with the best priority under `PREDICATE` from the
queue. On success, return three values: element, priority and T. If
the queue is already empty, return no values.

### `(HPQUEUE-FRONT queue)`

Peek the element with the best priority under `PREDICATE` from the
queue. On success, return three values: element, priority and T. If
the queue is empty, return no values.

### `(HPQUEUE-EMPTY queue)`

Return true if QUEUE is empty.

### `(HPQUEUE-COUNT queue)`

Return element count in QUEUE.

### `(HPQUEUE-DELETE element queue)`

Delete element from QUEUE, return its old priority, with secondary
return value indicating its presence.

### `(HPQUEUE-P queue)`

Type predicate for HPQUEUE structure

### `(COPY-HPQUEUE queue)`

Make a shallow copy of the QUEUE, with elements and priorities
remaining EQL to the original.

### `(CLEAR-HPQUEUE queue)`

Clear the QUEUE, not shrinking any reserved space. It could be more
efficient than making a fresh queue if it's filled repeatedly by a
large number of elements. On the other hand, it can waste memory if
there was an unusual one-time spike in the number of elements.

Return QUEUE.

### `(MAP-HPQUEUE function queue)`

Call FUNCTION with each element and its priority for all elements in
QUEUE, in no particular order. Return NIL.

### `(MERGE-HPQUEUE join-function queue &rest more-queues)`

Make a fresh queue by merging several existing queues, applying
two-argument JOIN-FUNCTION sequentially to calculate new element
priority if it occurs more than once.

### `(HASH-TABLE-HPQUEUE hash-table &optional (predicate #'<))`

Convert a hash table (elements as keys, priorities as values) to an
HPQUEUE, initializing queue TEST from HASH-TABLE, using a given
PREDICATE.

For big enough count of elements and priorities, it's more efficient
than adding each element one-by-one to an initially empty queue.

### `(HPQUEUE-HASH-TABLE queue)`

Convert the QUEUE to hash table, where elements are keys and
priorities are values. Hash table equality test is taken from the
QUEUE's.

### `(HPQUEUE-ALIST queue)`

Convert the QUEUE to an associative list of (cons element priority),
in no particular order.

## Design Explained

Heap-based queues are easy to implement, even ad-hoc, if you just push
and pop and never adjust. What if we have such a naïve implementation,
can we just add a hash table keeping track of each element position,
updating it when a priority changes and the element is moved? It turns
out not to be easy, because the element is not moved in a vacuum, and
positions of other elements may change as well. Sift-ups and
sift-downs, therefore, have to invoke a callback to update element
positions along the way, the interface becomes a mess, and the whole
adjustment operation ends up doing Θ(log(n)) hash lookups in the worst
case.

There's a programming contest at <https://adventofcode.com>, and I've
recently observed how people (and I) implement Dijkstra's algorithm in
Common Lisp when it's needed. The queue itself may be an ad-hoc
solution or a widely-known package (like `PRIORITY-QUEUE` or a
fibonacci queue from `QUEUES`), but the common approach to priority
adjustment is: Let there be duplicates, and when they're popped out,
I'll skip them as already visited anyway. That's what I decided to do
as well, after a half-baked attempt to implement position tracking in
an external hash table not integrated into the queue data structure.

So I decided to make a priority queue packages suitable for Dijkstra,
with the following ‘axiom’ in mind: position-tracking hash table
should be an integral part of my data structure. Every other decision
is a consequence.

### Intrinsic vs External Priorities

Priority of an element may be considered its property, instantly
available when you have an element itself, or it may be local to a
specific queue, so the element itself can have different priorities
within different queue instances. Elements being hash table keys
strongly suggest the latter: the whole point of my package is that
priorities are mutable, while hash table keys shouldn't be.

### Avoiding Unnecessary Hash Lookups

Hash table inside HPQUEUE maps element identities not directly to
their positions in the heap, but to a mutable `NODE` structure. Nodes
that populate the underlying arrary keep track of their own
positions. During sift-ups and sift-downs, positions are adjusted
without any access to the hash table.

### Priority as a SETFable Place

Following the hash table interface, I've made element priority
SETFable, and thus also useful for modifier macros like INCF and
DECF. However, exacly as in `(INCF (GETHASH key table 0))`, the hash
lookup happens twice during `(INCF (HPQUEUE-PRIORITY element queue
0))`. It's impossible to avoid it with Common Lisp design of SETF
functions and even macros: while intermediate arguments for the
specific SETF implementation are evaluated once if you're careful,
calls to `fn` and `(SETF fn)` themselves are always independent. SETF
expander could probably jump through some hoops and special-case
inplace updates, but I'm not sure if it can be done universally, and
it would not be easy to understand and maintain anyway.

Many lispers have some good additions to Common Lisp standard in
mind. Mine would be the notion of ‘in-place updater macro/function’,
which can be defined additionally to `fn` and `(SETF fn)`, so modifier
macros could call it with a closure that evaluates a new value from
the old one.

### In-place Update with `HPQUEUE-PUSH`

It's intuitive for a queue package to have a push function, even if we
don't exactly *need* it to populate the queue if we have `(SETF
HPQUEUE-PRIORITY)` already.

Our push function has an additional optional argument, JOIN-FUNCTION,
that is called for duplicate elements to calculate a new established
priority from the old one and the new proposed priority. Pushing the
same element with the same function and multiple priorities has an
effect like calling `reduce` on the sequence of priorities.

This facility leads to clean and unrestandable code when the function
is both commutative when called with two arguments and an identity
when called with one (think of `+`, `MIN`, `MAX`).

As an illustration, here's an example setting the element priority to
a minimum of its old priority and 1500, and adding it with a priority
of 1500 if it's not there yet (needed typically in Dijkstra's
algorithm).

```common-lisp
  (hpqueue-push "element" 1500 queue 'min)
```

Here's two ways of adding 200 to element priority *or* creating it
with priority 200 if it's not there yet:

```common-lisp
  (hpqueue-push "element" 200 queue '+)

  ;; hash lookup happens twice here
  (incf (hpqueue-priority "element" queue 0) 200)
```

## License

Public Domain
