(defpackage :gt-db
  (:use :common-lisp :sqlite :bordeaux-threads)
  (:export :defun/lock))
(in-package :gt-db)

(defvar *path* (concatenate 'string (sb-posix:getenv "HOME") "/gtgm.sqlite"))
(defvar *db* (sqlite:connect *path* :busy-timeout 1000))
(defvar *lock* (bordeaux-threads:make-recursive-lock "db lock"))

(defmacro lock/execute (function &body body)
  `(bordeaux-threads:with-recursive-lock-held (*lock*)
     (,function *db* ,@body)))

(defmacro lock/execute-non-query (&body body)
  `(lock/execute sqlite:execute-non-query ,@body))

(defmacro lock/execute-single (&body body)
  `(lock/execute sqlite:execute-single ,@body))

(defmacro lock/execute-to-list (&body body)
  `(lock/execute sqlite:execute-to-list ,@body))

(defmacro defun/lock (name lambda-list &body body)
  `(defun ,name ,lambda-list
     (bordeaux-threads:with-recursive-lock-held (*lock*)
       ,@body)))
