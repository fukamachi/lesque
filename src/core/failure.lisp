#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque.failure
  (:use :cl
        :annot.class)
  (:import-from :lesque.exception
                :exception)
  (:import-from :lesque.coder
                :coder
                :get-default-coder
                :decode)
  (:import-from :alexandria
                :symbolicate
                :delete-from-plist))
(in-package :lesque.failure)

(cl-syntax:use-syntax :annot)

@export
(defparameter *default-failure-backend* :redis)

(defun backend-class (backend)
  (let ((package-name (symbolicate :lesque.failure #\. backend))
        (class-name (symbolicate :failure #\- backend)))
    (intern (symbol-name class-name) package-name)))

@export
(defun make-failure (&rest initargs &key (backend *default-failure-backend*) &allow-other-keys)
  (setf initargs
        (delete-from-plist initargs :backend))
  (apply #'make-instance
         (backend-class backend)
         initargs))

@export
@export-accessors
(defclass base-failure ()
  ((exception :type exception
              :initarg :exception
              :accessor failure-exception)
   (queue :type string
          :initarg :queue
          :accessor failure-job-queue)
   (payload :type hash-table
            :accessor failure-payload)
   (coder :type coder
          :initarg :coder
          :initform (get-default-coder)
          :accessor failure-coder)))

(defmethod initialize-instance :after ((failure base-failure) &key payload &allow-other-keys)
  (when (stringp payload)
    (setf (failure-payload failure)
          (decode (failure-coder failure) payload))))

@export
(defgeneric save-failure (failure))

@export
(defgeneric backend-count-failures (failure-class))

@export
(defgeneric backend-all-failures (failure-class &optional offset limit))

@export
(defgeneric backend-clear-failures (failure-class))

@export
(defgeneric backend-requeue-failure (failure-class index))

@export
(defgeneric backend-remove-failure (failure-class index))

@export
(defun count-failures (&key (backend *default-failure-backend*))
  (backend-count-failures (find-class (backend-class backend))))

@export
(defun all-failures (&rest args &key (backend *default-failure-backend*) &allow-other-keys)
  (setf args (delete-from-plist args :backend))
  (apply #'backend-all-failures
         (find-class (backend-class backend))
         args))

@export
(defun clear-failures (&key (backend *default-failure-backend*))
  (backend-clear-failures (find-class (backend-class backend))))

@export
(defun requeue-failure (index &key (backend *default-failure-backend*))
  (backend-requeue-failure (find-class (backend-class backend))
                           index))

@export
(defun remove-failure (index &key (backend *default-failure-backend*))
  (backend-remove-failure (find-class (backend-class backend))
                          index))
