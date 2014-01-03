#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque.exception
  (:use :cl
        :annot.class)
  (:import-from :trivial-backtrace
                :print-backtrace-to-stream))
(in-package :lesque.exception)

(cl-syntax:use-syntax :annot)

@export
(define-condition exception (simple-error)
  ((condition :type condition
              :initarg :condition
              :reader exception-condition)
   (backtrace :type string
              :initarg :backtrace
              :reader exception-backtrace))
  (:report
   (lambda (condition stream)
     (format stream (error-string condition)))))
@export 'exception-condition
@export 'exception-backtrace

@export
(defmethod error-string ((exception exception))
  (princ-to-string (exception-condition exception)))

(defmethod initialize-instance :before ((obj exception) &key)
  (unless (slot-boundp obj 'backtrace)
    (setf (slot-value obj 'backtrace)
          (with-output-to-string (s)
            (print-backtrace-to-stream s)))))

@export
(defmethod exception-name ((exception exception))
  (class-name
   (class-of
    (exception-condition exception))))

@export
(defmacro make-exception (condition)
  `(make-condition 'exception
                   :condition ,condition
                   :backtrace (with-output-to-string (s)
                                (print-backtrace-to-stream s))))

@export
(defmacro with-backtrace-condition (&body body)
  `(handler-bind ((error
                    #'(lambda (condition)
                        (let ((exception (make-exception condition)))
                          (error exception)))))
     ,@body))
