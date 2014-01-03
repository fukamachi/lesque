#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque.backend
  (:use :cl)
  (:import-from :lesque.connection
                :connection
                :with-redis-connection
                :connection-coder)
  (:import-from :lesque.queue
                :queue
                #+nil :queue-size
                :slice-queue
                :destroy-queue)
  (:import-from :lesque.coder
                :decode)
  (:import-from :lesque.util
                :redis-key
                :emit-redis-prefix)
  (:import-from :red
                :smembers
                :lindex
                :lrange
                #+nil :keys
                :rpush))
(in-package :lesque.backend)

(cl-syntax:use-syntax :annot)

@export
(defmethod enqueue-to-queue ((conn connection) queue payload)
  (with-redis-connection conn
    (red:rpush (redis-key "queue" queue)
               payload)))

@export
(defmethod all-queues ((conn connection))
  (with-redis-connection conn
    (red:smembers (redis-key "queues"))))

@export
(defmethod queue-size ((conn connection) queue)
  (with-redis-connection conn
    (lesque.queue:queue-size (find-queue queue))))

@export
(defmethod peek-queue ((conn connection) queue &optional (start 0) (count 1))
  (with-redis-connection conn
    (slice-queue (find-queue queue) start count)))

@export
(defmethod list-range ((conn connection) key &optional (start 0) (count 1))
  (with-redis-connection conn
    (labels ((decode-object (object)
               (decode (connection-coder conn) object)))
      (if (= count 1)
          (list (decode-object (red:lindex key start)))
          (mapcar #'decode-object (red:lrange key start (1- (+ start count))))))))

(let ((queue-hash (make-hash-table :test 'equal)))
  @export
  (defmethod remove-queue ((conn connection) queue)
    (with-redis-connection conn
      (destroy-queue (find-queue queue)))
    (remhash queue queue-hash))

  @export
  (defun find-queue (queue)
    (multiple-value-bind (value present-p)
        (gethash queue queue-hash)
      (if present-p
          value
          (setf (gethash queue queue-hash)
                (make-instance 'queue :name queue))))))

@export
(defmethod keys ((conn connection))
  (with-redis-connection conn
    (mapcar #'emit-redis-prefix
            (red:keys "*"))))
