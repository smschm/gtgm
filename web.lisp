;(eval-when (:compile-toplevel)
;  (ql:quickload :sqlite)
;  (ql:quickload :html-template)
;  (ql:quickload :hunchentoot)
;  (ql:quickload :ironclad))

(defpackage :gt-web
  (:use :common-lisp));  :hunchentoot :sqlite))
(in-package :gt-web)

(defvar *path* (concatenate 'string (sb-posix:getenv "HOME") "/test.sql"))
(defvar *db* (sqlite:connect *path*))

(eval-when (:compile-toplevel) (format t "compile-toplevel"))
(eval-when (:load-toplevel) (format t "load-toplevel"))
(eval-when (:execute) (format t "execute"))

;;;; login / dologin

(defun hash-password (p)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array p))))

(defun login-success-p (name pw)
  (let ((hash (sqlite:execute-single *db*
		 "select pwhash from users where name = ?;" name)))
    (and hash (equal hash (hash-password pw)))))

(defun make-session-key (&key (len 32))
    (let ((hexes "0123456789abcdef"))
      (map 'string #'identity
	   (loop for i below len collect
		(elt hexes (random 16))))))

(defun /dologin (uname upw)
  (let ((successp (login-success-p uname upw))
	(session-key (make-session-key)))
    (if successp (progn
		   (hunchentoot:set-cookie "session" :value session-key)
		   (hunchentoot:set-cookie "uname" :value uname)
		   (sqlite:execute-non-query
		    *db* "update users set session = ? where name = ?;"
		    session-key uname)))
    (with-output-to-string (s)
      (html-template:fill-and-print-template
       #p"html/dologin.html"
       (list :success successp) :stream s))))

(defun add-login-to-handlers ()
  (push (hunchentoot:create-static-file-dispatcher-and-handler
	 "/login" #p"html/login.html" "text/html")
	hunchentoot:*dispatch-table*)
  (hunchentoot:define-easy-handler (dologin :uri "/dologin") (uname userpw)
    (setf (hunchentoot:content-type*) "text/html")
    (/dologin uname userpw)))

;;; npc management

(defun session-verify ()
  (let* ((uname (hunchentoot:cookie-in "uname"))
	 (session-key (hunchentoot:cookie-in "session"))
	 (session-stored
	  (sqlite:execute-single *db* "select session from users where name = ?;" uname)))
    (if (and uname session-key session-stored (equal session-key session-stored))
	uname nil)))

(defun npc/result-to-npc-templatable (uname res)
  (list :uname uname :rows (mapcar (lambda (n) (list :npcid (first n)
						     :npcname (fifth n)
						     :npcdesc (sixth n)
						     :npchost (third n)
						     :npcactive (equal (eighth n) 1)
						     :npcport (fourth n)))
			    res)))


(defun npc/query-to-html (template-path query stream)
  (html-template:fill-and-print-template template-path
       (result-to-npc-templatable
	 (sqlite:execute-to-list *db* query))
       ()))

(defun /npc (uname)
  (with-output-to-string (s)
    (html-template:fill-and-print-template
     #p"html/npc.html"
     (npc/result-to-npc-templatable uname
       (sqlite:execute-to-list *db* "select * from npcs where owner = ?;" uname))
     :stream s)))

(defun add-npc-to-handlers ()
  (hunchentoot:define-easy-handler (npc :uri "/npc") ()
    (setf (hunchentoot:content-type*) "text/html")
    (let ((uname (session-verify)))
      (if uname (/npc uname)))))

(defun /doaddnpc (uname npcname npcdesc npchost npcport npcactive)
  (let ((success nil) (err-message nil))
    (handler-case
	(progn (format t "npcactive: ~A~%" npcactive)
	  (sqlite:execute-non-query *db*
		"insert into npcs (owner, host, port, name, description, rating, active)
                 values (?, ?, ?, ?, ?, ?, ?);" uname npchost npcport npcname npcdesc
                 1000.0d0 (if (equal npcactive "yes") 1 0))
	       (setf success t))
      (sqlite:sqlite-constraint-error (e)
	(declare (ignore e))
	(setf err-message "Bot name already taken?")))
    (with-output-to-string (s)
      (html-template:fill-and-print-template
       #p"html/doaddnpc.html" (list :success success :reason err-message)
       :stream s))))


(defun add-addnpc-to-handlers ()
  (push (hunchentoot:create-static-file-dispatcher-and-handler
	 "/addnpc" #p"html/addnpc.html" "text/html")
	hunchentoot:*dispatch-table*)
  (hunchentoot:define-easy-handler (doaddnpc :uri "/doaddnpc")
      (npcname npcdesc npchost (npcport :parameter-type 'integer) npcactive)
    (let ((uname (session-verify)))
      (if uname (/doaddnpc uname npcname npcdesc
			   (if (equal npchost "") nil npchost)
			   npcport npcactive)))))
    
;;;;

(defun /npcedit (uname id)
  (let* ((id-owner (sqlite:execute-single *db* "select owner from npcs where id = ?;" id))
	 (npc-data-l (sqlite:execute-to-list *db* "select * from npcs where id = ?;" id))
	 (valid (and uname id id-owner (equal id-owner uname))))
    (if valid
	(with-output-to-string (s)
	  (html-template:fill-and-print-template
	   #p"html/npcedit.html"
	   (let ((npc-data (car npc-data-l)))
	     (list :valid t :npcname (fifth npc-data)
		   :curdesc (sixth npc-data)
		   :curhost (third npc-data)
		   :curport (fourth npc-data)
		   :id id)) :stream s))
	(with-output-to-string (s)
	  (html-template:fill-and-print-template
	   #p"html/npcedit.html" (list :valid nil :uname uname :id id)
	  :stream s)))))

;(defun /donpcedit (uname id newdesc newhost newport))

(defun add-npcedit-to-handlers ()
  (hunchentoot:define-easy-handler (npcedit :uri "/npcedit") ((id :parameter-type 'integer))
    (setf (hunchentoot:content-type*) "text/html")
    (let ((uname (session-verify)))
      (if uname (/npcedit uname id)))))

;;;;

(defun add-logout-to-handlers ()
  (hunchentoot:define-easy-handler (logout :uri "/logout") ()
    (let ((uname (session-verify)))
      (if uname (sqlite:execute-non-query *db* "update users set session = '' where name = ?;"
					  uname)))))

;;;;

(defun h-start ()
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor :port 4242)))
