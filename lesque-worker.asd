#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque-worker-asd
  (:use :cl :asdf))
(in-package :lesque-worker-asd)

(defsystem lesque-worker
  :version "0.1"
  :author "Eitarow Fukamachi"
  :license "MIT"
  :depends-on (:cl-syntax-annot
               :lesque
               :cl-redis
               :osicat
               :cffi
               :local-time
               :trivial-shell
               :log4cl
               :split-sequence
               :alexandria)
  :components ((:module "src/worker"
                :components
                ((:file "worker" :depends-on ("registry" "child-process" "signal-handling"))
                 (:file "registry" :depends-on ("process-coordinator"))
                 (:file "process-coordinator")
                 (:file "child-process" :depends-on ("signal-handling" "util"))
                 (:file "signal-handling")
                 (:file "util")))))
