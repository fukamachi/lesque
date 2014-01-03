#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque-worker.signal-handling
  (:use :cl)
  (:nicknames :lesque.worker.signal-handling)
  (:import-from :cffi
                :defcallback
                :foreign-funcall
                :callback))
(in-package :lesque-worker.signal-handling)

(cl-syntax:use-syntax :annot)

(defvar *signal-handlers* (make-hash-table))

(cffi:defcallback signal-handler :void ((signo :int))
  (declare (ignore signo))
  (when (functionp (gethash signo *signal-handlers*))
    (funcall (gethash signo *signal-handlers*))))

@export
(defun set-signal-handler (signo fn)
  (multiple-value-bind (value present-p) (gethash signo *signal-handlers*)
    (setf (gethash signo *signal-handlers*) fn)
    (unless present-p
      (cffi:foreign-funcall "signal" :int signo :pointer (cffi:callback signal-handler)))))

@export
(defun unset-signal-handler (signo)
  (remhash signo *signal-handlers*)
  (cffi:foreign-funcall "signal" :int signo :unsigned-long 0))
