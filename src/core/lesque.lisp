#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque
  (:use :cl)
  (:import-from :lesque.connection
                :*connection*
                :connection
                :connect
                :reconnect
                :disconnect)
  (:import-from :lesque.backend
                :all-queues
                :queue-size
                :peek-queue
                :list-range
                :remove-queue
                :find-queue
                :keys)
  (:import-from :lesque.job
                :job
                :function-job
                :perform
                :enqueue-job
                :fail-job
                :job-payload
                :default-queue-name
                :make-job)
  (:import-from :lesque.failure
                :count-failures
                :all-failures
                :clear-failures
                :requeue-failure
                :remove-failure)
  (:import-from :lesque.coder
                :*default-coder-class*)
  (:import-from :lesque.util
                :*lesque-namespace*
                :symbol-to-string)
  (:export :*connection*
           :connect
           :reconnect
           :disconnect
           :all-queues
           :queue-size
           :peek-queue
           :list-range
           :remove-queue
           :find-queue
           :keys
           :job
           :function-job
           :perform
           :enqueue-job
           :fail-job
           :job-payload
           :default-queue-name
           :count-failures
           :all-failures
           :clear-failures
           :requeue-failure
           :remove-failure
           :*default-coder-class*
           :*lesque-namespace*))
(in-package :lesque)

(cl-syntax:use-syntax :annot)

@export
(defgeneric enqueue (connection queue job-or-job-class &optional args))

(defmethod enqueue ((connection connection) queue (job job) &optional args)
  (declare (ignore queue args))
  (enqueue-job connection job))

(defmethod enqueue ((connection connection) queue (job-class symbol) &optional args)
  (enqueue connection queue
          (make-instance job-class
                         :args args
                         :queue queue)))

(defmethod enqueue ((connection connection) queue (function-job-list list) &optional args)
  (declare (ignore args))
  (enqueue connection queue 'function-job
           `(,(symbol-to-string (car function-job-list))
             ,@(cdr function-job-list))))
