;;;; -*- Mode: Lisp -*-

(asdf:defsystem #:gtgm
  :serial t
  :depends-on (#:s-xml-rpc #:bordeaux-threads #:trivial-timeout
               #:hunchentoot #:sqlite #:ironclad #:html-template)
  :components ((:file "db")
	       (:file "game")
               (:file "elo")
               (:file "web")
               (:file "supervisor")))
