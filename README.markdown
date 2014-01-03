# Lesque

Redis-backed library for creating background jobs, placing them on multiple queues, and processing them later. Lesque is highly inspired [Resque](https://github.com/resque/resque) in Ruby.

## Usage

    (defvar *lesque* (lesque:connect :host "localhost" :port 6379))
    
    (defun deferred-job (&rest args)
      ;; blah blah blah
      )
    
    (lesque:enqueue *lesque*
                    "my-queue"
                    '(deferred-job "arg1" "arg2"))
    
    (lesque.worker:run "my-queue")

## TODO

* Web interface

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the MIT License. See LICENSE file.
