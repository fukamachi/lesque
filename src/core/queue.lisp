#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque.queue
  (:use :cl)
  (:import-from :lesque.coder
                :coder
                :get-default-coder
                :encode
                :decode)
  (:import-from :lesque.util
                :redis-key)
  (:import-from :red
                :sadd
                :rpush
                :lpop
                :blpop
                :lindex
                :lrange))
(in-package :lesque.queue)

(cl-syntax:use-syntax :annot)

@export
(defclass queue ()
  ((name :type string
         :initarg :name
         :reader queue-name)
   (coder :type coder
          :initarg :coder
          :initform (get-default-coder)
          :accessor queue-coder)
   (destroyed-p :type boolean
                :initform nil
                :accessor queue-destroyed-p)))
@export 'queue-destroyed-p

(defmethod initialize-instance :after ((queue queue) &key)
  (red:sadd (redis-key "queues")
            (queue-name queue)))

(defmethod redis-name ((queue queue))
  (redis-key "queue" (queue-name queue)))

@export
(defmethod push-queue ((queue queue) object)
  (when (queue-destroyed-p queue)
    (error "Queue is already destroyed."))

  (let ((encoded-object (encode-object queue object)))
    (red:rpush (redis-name queue)
               encoded-object)))

@export
(defmethod pop-queue ((queue queue) &key (non-block nil))
  (if non-block
      (decode-object queue (red:lpop (redis-name queue)))
      (loop with last-value = nil
            for value = (red:blpop (redis-name queue) 1)
            while value
            do (setf last-value value)
            finally
               (return (decode-object queue last-value)))))

@export
(defmethod slice-queue ((queue queue) start length)
  (labels ((decode (object)
             (decode-object queue object)))
    (if (= length 1)
        (decode (red:lindex (redis-name queue) start))
        (mapcar #'decode
                (red:lrange (redis-name queue) start (1- (+ start length)))))))

@export
(defmethod queue-size ((queue queue))
  (red:llen (redis-name queue)))

@export
(defmethod queue-empty-p ((queue queue))
  (= (queue-size queue) 0))

@export
(defmethod destroy-queue ((queue queue))
  (redis:with-pipelining
    (red:del (redis-name queue))
    (red:srem (redis-key "queues")
              (queue-name queue)))
  (setf (queue-destroyed-p queue) t))

@export
(defmethod encode-object ((queue queue) object)
  (encode (queue-coder queue) object))

@export
(defmethod decode-object ((queue queue) object)
  (decode (queue-coder queue) object))
