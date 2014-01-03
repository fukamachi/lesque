#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque.util
  (:use :cl))
(in-package :lesque.util)

(cl-syntax:use-syntax :annot)

@export
(defvar *lesque-namespace* "resque")

@export
(defun redis-key (&rest args)
  (format nil "~A:~{~A~^:~}"
          *lesque-namespace*
          args))

@export
(defun emit-redis-prefix (key)
  (if (string= key *lesque-namespace*
               :end1 (length *lesque-namespace*))
      (subseq key (length *lesque-namespace*))
      key))

@export
(defun symbol-to-string (symbol)
  (check-type symbol symbol)
  (let ((package-name (package-name (symbol-package symbol))))
    (format nil
            "~:[~A::~;~*~]~A"
            (or (eq package-name 'cl)
                (eq package-name 'cl-user))
            package-name
            (symbol-name symbol))))
