;(eval-when (:compile-toplevel)
;  (ql:quickload :sqlite)
;  (ql:quickload :html-template)
;  (ql:quickload :hunchentoot)
;  (ql:quickload :ironclad))

(defpackage :gt-web
  (:use :common-lisp :gt-db)
  (:import-from :gt-db :*db* :defun/lock))
(in-package :gt-web)

(defvar *acceptor*)

(defmacro sql-execute-single-or-nil (&body params)
  `(handler-case (sqlite:execute-single *db* ,@params)
     (sqlite:sqlite-error (c)
       (declare (ignore c))
       nil)))

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

(defun make-session-key (&key (len 12))
    (let ((hexes "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
      (map 'string #'identity
	   (loop for i below len collect
		(elt hexes (random (length hexes)))))))

(defun/lock /dologin (uname upw)
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

(defun/lock session-verify ()
  (let* ((uname (hunchentoot:cookie-in "uname"))
	 (session-key (hunchentoot:cookie-in "session"))
	 (session-stored (sql-execute-single-or-nil 
                       "select session from users where name = ?;" uname)))
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

(defun/lock /npc (uname)
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

(defun/lock /doaddnpc (uname npcname npcdesc npchost npcport npcactive)
  (let ((success nil) (err-message nil))
    (handler-case
	(progn (format t "npcactive: ~A~%" npcactive)
	  (sqlite:execute-non-query *db*
		"insert into npcs (owner, host, port, name, description, rating, active)
                 values (?, ?, ?, ?, ?, ?, ?);" uname npchost npcport
                 ;(hunchentoot:escape-for-html npcname)
                 ;(hunchentoot:escape-for-html npcdesc)
                 (string-trim " " npcname)
                 (string-trim " " npcdesc)
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

(defun/lock /npcedit (uname id)
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

(defun/lock /donpcedit (uname id newdesc newhost newport)
  (let* ((id-owner (sqlite:execute-single *db* "select owner from npcs where id = ?;" id))
         (active-p (sqlite:execute-single *db* "select active from npcs where id = ?;" id))
         (valid (and uname id id-owner active-p (equal id-owner uname))))
    (if valid (sqlite:execute-non-query *db*
                  "update npcs set host = ?, port = ?, description = ? where id = ?;"
                  newhost newport (string-trim " " newdesc) id))
    (with-output-to-string (s)
      (html-template:fill-and-print-template
       #p"html/donpcedit.html" `(:success ,valid)
     :stream s))))

(defun add-npcedit-to-handlers ()
  (hunchentoot:define-easy-handler (npcedit :uri "/npcedit") ((id :parameter-type 'integer))
    (setf (hunchentoot:content-type*) "text/html")
    (let ((uname (session-verify)))
      (if uname (/npcedit uname id))))
  (hunchentoot:define-easy-handler (donpcedit :uri "/donpcedit")
      ((id :parameter-type 'integer) newdesc newhost (newport :parameter-type 'integer))
    (setf (hunchentoot:content-type*) "text/html")
    (let ((uname (session-verify)))
      (if uname (/donpcedit uname id newdesc newhost newport)))))
;;;;

(defun/lock /leaderboard ()
  (let ((results (sqlite:execute-to-list *db* "select * from npcs where active = 1 order by rating desc;")))
    (with-output-to-string (s)
      (html-template:fill-and-print-template
       #p"html/leaderboard.html" `(:rows ,(loop for npc in results for rank from 1 collect
                                               `(:rank ,rank :rating ,(floor (seventh npc))
                                                 :npcname ,(fifth npc)
                                                 :owner ,(second npc)
                                                 :npcdesc ,(sixth npc)
						 :ping ,(if (equal (ninth npc) 1) "*" ""))))
       :stream s))))

(defun add-leaderboard-to-handlers ()
  (hunchentoot:define-easy-handler (leaderboard :uri "/leaderboard") ()
    (setf (hunchentoot:content-type*) "text/html")
    (/leaderboard)))

;;;;

(defun/lock /user (uname)
  (let ((host (if uname
                  (sqlite:execute-single *db* "select host from users where name = ?;" uname)
                  nil)))
    (with-output-to-string (s)
      (html-template:fill-and-print-template
       #p"html/user.html" `(:loggedin ,uname :uname ,uname :uhost ,host) :stream s))))



(defun/lock /douser (uname curpw newpw newpw2 host)
  (let ((valid (and uname (equal (hash-password curpw)
                                 (sql-execute-single-or-nil
                                   "select pwhash from users where name = ?;" uname))))
        (pw-updated nil))
    (if valid
        (progn (sqlite:execute-non-query *db*
                  "update users set host = ? where name = ?;"
                  host uname)
               (if (and (not (equal newpw "")) (equal newpw newpw2))
                   (progn (sqlite:execute-non-query *db*
                             "update users set pwhash = ? where name = ?;"
                             (hash-password newpw) uname)
                          (setf pw-updated t)))))
    (with-output-to-string (s)
      (html-template:fill-and-print-template
       #p"html/douser.html" `(:valid ,valid :pwupdated ,pw-updated) :stream s))))

(defun add-user-to-handlers ()
  (hunchentoot:define-easy-handler (user :uri "/user") ()
    (setf (hunchentoot:content-type*) "text/html")
    (let ((uname (session-verify)))
      (/user uname)))
  (hunchentoot:define-easy-handler (douser :uri "/douser") (curpw newpw newpw2 host)
    (setf (hunchentoot:content-type*) "text/html")
    (/douser (session-verify) curpw newpw newpw2 host)))

;;;;

(defun/lock /npctoggle (uname id)
  (let* ((id-owner (sqlite:execute-single *db* "select owner from npcs where id = ?;" id))
         (active-p (sqlite:execute-single *db* "select active from npcs where id = ?;" id))
         (valid (and uname id id-owner active-p (equal id-owner uname)))
         (new-active (- 1 active-p)))
    (if valid
        (sqlite:execute-non-query *db* "update npcs set active = ? where id = ?;" new-active id))
	(with-output-to-string (s)
	  (html-template:fill-and-print-template
	   #p"html/npctoggle.html"
	   `(:valid ,valid :newactive ,(equal new-active 1)) :stream s))))

(defun add-npctoggle-to-handlers ()
  (hunchentoot:define-easy-handler (npctoggle :uri "/npctoggle") ((id :parameter-type 'integer))
    (setf (hunchentoot:content-type*) "text/html")
    (/npctoggle (session-verify) id)))

;;;;

(defun add-logout-to-handlers ()
  (hunchentoot:define-easy-handler (logout :uri "/logout") ()
    (let ((uname (session-verify)))
      (if uname (sqlite:execute-non-query *db* "update users set session = '' where name = ?;"
					  uname)))))

;;;;

(defun add-root-to-handlers ()
  (hunchentoot:define-easy-handler (root :uri "/") ()
    (setf (hunchentoot:content-type*) "text/html")
    (let ((uname (session-verify)))
      (with-output-to-string (s)
        (html-template:fill-and-print-template
         #p"html/index.html" `(:loggedin ,uname :uname ,uname)
         :stream s)))))

;;;;

(defun/lock win-percentage (id0 id1)
  (let* ((wins (sqlite:execute-single *db* "select count(*) from games where (id0 = ? and id1 = ? and score1 < score0) or (id0 = ? and id1 = ? and score1 > score0);" id0 id1 id1 id0))
         (losses (sqlite:execute-single *db* "select count(*) from games where (id0 = ? and id1 = ? and score1 < score0) or (id0 = ? and id1 = ? and score1 > score0);" id1 id0 id0 id1))
         (total (+ wins losses)))
    (if (= total 0) nil
        (round (* 1000 (/ wins (+ wins losses)))))))

(defun/lock /winmatrix ()
  (let* ((active (sqlite:execute-to-list *db* "select id, name from npcs where active = 1;")))
    `(:headings ,(loop for npc in active collect `(:id ,(first npc)))
      :rows ,(loop for npc in active collect
                  `(:name ,(second npc) :id ,(first npc) :columns
                          ,(loop for npc2 in active collect
                                `(:count ,(win-percentage (first npc) (first npc2)))))))))

(defun add-winmatrix-to-handlers ()
  (hunchentoot:define-easy-handler (winmatrix :uri "/winmatrix") ()
    (setf (hunchentoot:content-type*) "text/html")
    (with-output-to-string (s)
      (html-template:fill-and-print-template
         #p"html/winmatrix.html" (/winmatrix)
         :stream s))))

(defun h-start ()
  (setf *acceptor*
        (hunchentoot:start
         (make-instance 'hunchentoot:easy-acceptor :port 4242)))
  (setf (hunchentoot:acceptor-error-template-directory *acceptor*) nil
        (hunchentoot:acceptor-document-root *acceptor*) #p"html/static/"))

(defun add-all-handlers ()
  (add-login-to-handlers)
  (add-logout-to-handlers)
  (add-npc-to-handlers)
  (add-npcedit-to-handlers)
  (add-npctoggle-to-handlers)
  (add-user-to-handlers)
  (add-root-to-handlers)
  (add-addnpc-to-handlers)
  (add-leaderboard-to-handlers))
