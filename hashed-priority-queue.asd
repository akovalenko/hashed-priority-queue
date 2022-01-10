;;;; hashed-priority-queue.asd

(asdf:defsystem #:hashed-priority-queue
  :description
  "This is a library providing heap-based priority queues, with an
additional feature of keeping track which element is where,
facilitating fast adjustment of existing element priorities."
  :author "Anton Kovalenko <anton@sw4me.com>"
  :license  "Public Domain"
  :version "1.0.0"
  :serial t
  :components ((:file "package")
               (:file "hashed-priority-queue"))
  :in-order-to ((asdf:test-op (asdf:test-op "hashed-priority-queue/test"))))

(asdf:defsystem "hashed-priority-queue/test"
  :depends-on ("hashed-priority-queue"
	       "fiveam"
	       "alexandria")
  :components ((:file "tests"))
  :perform (asdf:test-op (o c)
			 (funcall (read-from-string "5am:run!")
				  :hashed-priority-queue)))
