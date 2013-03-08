(defpackage :gt-super
  (:use :common-lisp :gt-game :s-xml-rpc :bordeaux-threads :sqlite)
  (:import-from :gt-db :*db* :*game-record* :defun/lock))
(in-package :gt-super)

(defconstant +timeout+ 5)
(defvar *continue*)

(define-condition player-error (error)
  ((offender :initarg :offender :accessor offender)
   (description :initarg :description :accessor description))
  (:report (lambda (condition stream)
             (format stream "~A" (description condition)))))

(defun player-call (player-uris player-num &rest call)
  (xml-rpc-call (apply #'encode-xml-rpc-call call)
                :host (car (elt player-uris player-num))
                :port (cdr (elt player-uris player-num))))

(defun player-call/ignore (&rest parms)
  (handler-case (trivial-timeout:with-timeout (+timeout+)
                  (apply #'player-call parms))
    (error (c)
      (declare (ignore c))
      nil)))

(defun player-call/handle (player-uris player-num &rest call)
  (handler-case (trivial-timeout:with-timeout (+timeout+)
                  (apply #'player-call (append (list player-uris player-num)
                                               call)))
    (trivial-timeout:timeout-error (c)
      (declare (ignore c))
      (error 'player-error :offender player-num :description
             (format nil "Player ~d exceeded time limit responding to: ~A"
                     player-num call)))
    (xml-rpc-fault (c)
      (error 'player-error :offender player-num :description
             (format nil "Player ~d returned an XML-RPC error responding to: ~A; message was: ~A"
                      player-num call (xml-rpc-fault-string c))))
    (error (e)
      (error 'player-error :offender player-num :description
             (format nil "Caught generic error from player ~d when calling: ~A; error: ~A"
                     player-num call e)))))
  

(defmacro with-xml-struct-bindings (v vars &rest body)
  `(let ,(loop for b in vars collect `(,b (get-xml-struct-member ,v ,b)))
     ,@body))

(defmacro princ-executing (&body expr)
  `(progn (princ (quote ,@expr)) ,@expr))

(defun card-to-struct (c) (xml-rpc-struct :|rank| (cdr c) :|suit| (car c)))
(defun cardlist-to-xml (cs) (map 'vector #'card-to-struct cs))
(defun cardlistvector-to-xml (csv) (map 'vector #'cardlist-to-xml csv))

; this is extremely ugly
(defun cardlistmatrix-to-xml (csm)
  (map 'vector #'identity
       (loop for i below 2 collect
            (map 'vector #'cardlist-to-xml
                 (loop for j below gt-game::+suits+ collect (aref csm i j))))))

(defun game-runner (player-uris)
  ; player uris should be of the form
  ; ((player0-uri . player0-port) (player1-uri . player1-port))
  (if (not (and (player-call/ignore player-uris 0 "startGame")
                (player-call/ignore player-uris 1 "startGame")))
      (return-from game-runner :game-declined))
  (let ((g (make-instance 'game-state)) (turn 0) (this-game-record nil))
    (start-game g)
    (push (list 
    (player-call/handle player-uris
                 0 "initialize" 0 0 ; opponent id = game id = 0 for now
                 0 (mapcar #'card-to-struct (elt (hands g) 0)))
    (player-call/handle player-uris
                 1 "initialize" 0 0 ; opponent id = game id = 0 for now
                 1 (mapcar #'card-to-struct (elt (hands g) 1)))
    (block outer (loop do
         (let* ((r0 (player-call/handle player-uris
                     turn "getPlay"
                     (cardlist-to-xml (elt (hands g) turn))
                     (cardlistvector-to-xml (discards g))
                     (cardlistmatrix-to-xml (expos g))
                     (length (deck g))))
                card-ix play-to-raw play-to draw-from card-played)
           
           ; bots are guilty until proven innocent
           (handler-case 
               (setf card-ix (get-xml-rpc-struct-member r0 :|card_ix|)
                     play-to-raw (get-xml-rpc-struct-member r0 :|play_to|)
                     play-to (if (= play-to-raw 0) :discard :expo)
                     draw-from (get-xml-rpc-struct-member r0 :|draw_from|)
                     card-played (elt (elt (hands g) turn) card-ix))
             (error (e)
	       (let ((error-description (with-output-to-string (s) (describe e s))))
		 (push (list :error turn error-description) this-game-record)
		 (push this-game-record *game-record*)
		 (error 'player-error :offender turn :description error-description))))

           ;(princ (list card-ix play-to draw-from))
           ;(describe g)
           (if (equalp :discarded-instead
                       (place-card g turn card-ix play-to))
               (setf play-to :discard))
           (if (and (equal play-to :discard)
                    (= (car card-played) draw-from))
               ; trying to draw the discarded card
               (progn (format t "trying to draw discarded!") (terpri)
                      (setf draw-from -1)))
           (if (equalp :drew-from-deck
                       (player-draw-card g turn
                                         (if (< draw-from 0) :deck :discard)
                                         :pile-ix draw-from))
               (setf draw-from -1))
	   (push (list turn card-played play-to draw-from) this-game-record)
           (player-call/ignore player-uris (- 1 turn) "opponentPlay" (card-to-struct card-played)
                        play-to-raw draw-from)
           (if (null (deck g)) (return-from outer nil))
           (setf turn (- 1 turn)))))
    (let ((scores (score-game g)))
      (push (list :end (car scores) (cadr scores)) this-game-record)
      (push this-game-record *game-record*)
      (player-call/ignore player-uris 0 "gameEnd" (car scores) (cadr scores))
      (player-call/ignore player-uris 1 "gameEnd" (car scores) (cadr scores))
      scores)))

(defun nil-or-empty (x)
  (or (null x) (equal x "")))

(defun/lock get-address-by-id (id)
  (let* ((owner (sqlite:execute-single gt-web::*db* "select owner from npcs where id = ?;" id))
	 (owner-host (sqlite:execute-single gt-web::*db* "select host from users where name = ?;" owner))
	 (npc-port (sqlite:execute-single gt-web::*db* "select port from npcs where id = ?;" id))
	 (npc-host (sqlite:execute-single gt-web::*db* "select host from npcs where id = ?;" id))
	 (host (if (nil-or-empty npc-host) owner-host npc-host)))
    (cons host npc-port)))

(defun play-by-id (id0 id1)
  (handler-case
      (game-runner (list (get-address-by-id id0)
			 (get-address-by-id id1)))
    (player-error (e)
      (list :error (offender e)))))

(defun test-ping (id)
  (let ((ping (player-call/ignore (list (get-address-by-id id)) 0 "ping")))
    (sqlite:execute-non-query gt-web::*db*
			      "update npcs set pinging = ? where id = ?;"
			      (if ping 1 0) id)
    ping))

(defun/lock check-pinging ()
  (let* ((active-npcs (sqlite:execute-to-list gt-web::*db*
			 "select id, name  from npcs where active = 1;")))
    (loop for id in active-npcs
	 do (progn 
	      (format t "checking #~A [~A]~%" (first id) (second id))
	      (test-ping (first id))))))

(defun/lock pinging-ids ()
  (mapcar #'car (sqlite:execute-to-list gt-web::*db*
		  "select id from npcs where pinging = 1;")))

(defun/lock update-elo (p0 p1 winner)
  (let* ((wonp (if (= winner 0) t
		   (if (= winner 1) nil :tie)))
	 (rating0
	  (sqlite:execute-single gt-web::*db*
				 "select rating from npcs where id = ?;" p0))
	 (rating1
	  (sqlite:execute-single gt-web::*db*
				 "select rating from npcs where id = ?;" p1))
	 (new (elo:new-rating rating0 rating1 wonp)))
    (sqlite:execute-non-query gt-web::*db*
	"update npcs set rating = ? where id = ?;" (car new) p0)
    (sqlite:execute-non-query gt-web::*db*
	"update npcs set rating = ? where id = ?;" (cdr new) p1)))

(defun main-runner ()
  (let* ((active (pinging-ids))
	 (count (length active))
	 (p0-ix (progn
		  (if (< count 2) (return-from main-runner :too-few))
		  (random count)))
	 (p1-ix (let ((r (random (- count 1))))
		  (+ r (if (>= r p0-ix) 1 0))))
	 (p0 (elt active p0-ix)) (p1 (elt active p1-ix))
	 (result (progn
		   (format t "playing ~A v ~A~%" p0 p1)
		   (play-by-id p0 p1)))
	 (winner nil) (result-code nil)
	 (score0 nil) (score1 nil))
    (cond
      ((null result) nil)
      ((equal result :game-declined) :game-declined)
      ((= (length result) 2)
       (progn (if (equal (first result) :error)
		  (setf result-code "error"
			winner (- 1 (second result))
			;; maps errorer to -1, non-errorer to 0:
			score0 (- (second result) 1) ; these are just
			score1 (- (second result))) ; being a dick
		  (if (= (first result) (second result))
		      (setf result-code "tie"
			    winner -1
			    score0 (first result)
			    score1 (second result))
		      (setf result-code "win"
			    winner (if (> (first result) (second result)) 0 1)
			    score0 (first result)
			    score1 (second result))))
	      (gt-db::lock/execute-non-query
		 "insert into games (id0, id1, result, score0, score1) values (?,?,?,?,?);"
		 p0 p1 result-code score0 score1)
	      (update-elo p0 p1 winner)))
      (t :guru-meditation))))

(defun main-loop () (loop for i from 0 do
                         (progn (sleep 2)
                                (format t "~A~%" i)
                                (check-pinging)
                                (main-runner))
                         while *continue*))
