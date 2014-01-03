#|
  This file is a part of lesque project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage lesque.stat
  (:use :cl)
  (:import-from :lesque.connection
                :*connection*
                :with-redis-connection)
  (:import-from :lesque.util
                :redis-key)
  (:import-from :red
                #+nil :get
                :incrby
                :decrby
                :del))
(in-package :lesque.stat)

(cl-syntax:use-syntax :annot)

@export
(defun get-stat (stat)
  (with-redis-connection *connection*
    (parse-integer
     (red:get (redis-key "stat" stat))
     :junk-allowed t)))

@export
(defun incr-stat (stat &key (by 1))
  (with-redis-connection *connection*
    (red:incrby (redis-key "stat" stat) by)))

@export
(defun decr-stat (stat &key (by 1))
  (with-redis-connection *connection*
    (red:decrby (redis-key "stat" stat) by)))

@export
(defun clear-stat (stat)
  (with-redis-connection *connection*
    (red:del (redis-key "stat" stat))))
