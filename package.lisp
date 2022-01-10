;;;; package.lisp

(defpackage #:hashed-priority-queue
  (:use #:cl)
  (:export
   ;; lifecycle
   #:make-hpqueue
   #:copy-hpqueue
   #:merge-hpqueue
   #:map-hpqueue
   #:clear-hpqueue

   ;; typecheck
   #:hpqueue
   #:hpqueue-p

   ;; usage
   #:hpqueue-priority
   #:hpqueue-push
   #:hpqueue-pushnew
   #:hpqueue-pop
   #:hpqueue-front
   #:hpqueue-empty
   #:hpqueue-count
   #:hpqueue-equal

   ;; conversion
   #:hpqueue-alist
   #:hpqueue-hash-table
   #:hash-table-hpqueue))
