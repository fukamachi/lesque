#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque-test
  (:use :cl
        :lesque
        :cl-test-more))
(in-package :lesque-test)

(plan 4)

(defparameter *connection*
  (connect :host "localhost"
           :port 6379))

(ok *connection*)

(remove-queue *connection* "my-queue")

(let ((job (make-instance 'function-job
                          :class 'function-job
                          :args `("LESQUE-TEST:ADD" 1 2 "three"))))
  (is (gethash "class" (yason:parse (job-payload job)))
      "LESQUE.JOB:FUNCTION-JOB")
  (is (gethash "args" (yason:parse (job-payload job)))
      '("LESQUE-TEST:ADD" 1 2 "three")))

(ok (enqueue *connection*
             "my-queue"
             '(add 1 2 "three")))

(finalize)

(disconnect *connection*)
