#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque.failure.redis
  (:use :cl
        :lesque.failure)
  (:import-from :lesque.connection
                :*connection*
                :with-redis-connection)
  (:import-from :lesque.backend
                :list-range)
  (:import-from :lesque.exception
                :exception-name
                :exception-backtrace
                :error-string)
  (:import-from :lesque.coder
                :encode
                :get-default-coder)
  (:import-from :lesque.util
                :redis-key
                :symbol-to-string)
  (:import-from :red
                :rpush
                :lrem
                :llen
                :lrange
                :del)
  (:import-from :local-time
                :format-timestring
                :now)
  (:import-from :alexandria
                :plist-hash-table))
(in-package :lesque.failure.redis)

(cl-syntax:use-syntax :annot)

@export
(defclass failure-redis (base-failure) ())

;;
;; Instance Methods

(defmethod save-failure ((failure failure-redis))
  (let ((data (plist-hash-table
               `("failed_at" ,(format-timestring nil (now))
                 "payload" ,(failure-payload failure)
                 "exception" ,(symbol-to-string (exception-name (failure-exception failure)))
                 "error" ,(error-string (failure-exception failure))
                 "backtrace" ,(exception-backtrace (failure-exception failure))
                 "queue" ,(failure-job-queue failure)))))
    (with-redis-connection *connection*
      (red:rpush (redis-key "failed")
                 (encode (failure-coder failure) data)))))

;;
;; Class Methods

(defmethod backend-count-failures ((failure-class (eql (find-class 'failure-redis))))
  (declare (ignore failure-class))
  (with-redis-connection *connection*
    (red:llen (redis-key "failed"))))

(defmethod backend-all-failures ((failure-class (eql (find-class 'failure-redis)))
                                 &optional (offset 0) (limit 1))
  (list-range *connection* (redis-key "failed")
              offset (1- (+ offset limit))))

(defmethod backend-clear-failures ((failure-class (eql (find-class 'failure-redis))))
  (with-redis-connection *connection*
    (red:del (redis-key "failed")))
  T)

(defmethod backend-requeue-failure ((failure-class (eql (find-class 'failure-redis))) index)
  (let ((item (first (backend-all-failures failure-class index))))
    (setf (gethash "retried_at" item)
          (format-timestring nil (now)))
    (with-redis-connection *connection*
      (red:lset (redis-key "failed")
                index
                (encode (get-default-coder) item))
      (red:rpush (gethash "queue" item)
                 (gethash "payload" item)))))

(defmethod backend-remove-failure ((failure-class (eql (find-class 'failure-redis))) index)
  (let ((sentinel ""))
    (with-redis-connection *connection*
      (red:lset (redis-key "failed")
                index
                sentinel)
      (red:lrem (redis-key "failed")
                1
                sentinel))))
