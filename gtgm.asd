;;;; -*- Mode: Lisp -*-

(asdf:defsystem #:gtgm
  :serial t
  :depends-on (#:s-xml-rpc #:bordeaux-threads #:trivial-timeout
               #:hunchentoot #:sqlite #:ironclad #:html-template)
  :components ((:file "game")
               (:file "web")
               (:file "elo")
               (:file "supervisor")))
