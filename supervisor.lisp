(defpackage :gt-super
  (:use :common-lisp :gt-game :s-xml-rpc :bordeaux-threads))
(in-package :gt-super)

(defparameter +timeout+ 5)

(define-condition player-error (error)
  ((offender :initarg :offender)
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
    (trivial-timeout:timeout-error (c)
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

  ; confirm both players are ready
  ;(if (not (xml-rpc-call (encode-xml-rpc-call "startGame")
  ;                       :host player0-uri :port player0-port))
  ;    (return-from game-runner nil))
  ;(if (not (xml-rpc-call (encode-xml-rpc-call "startGame")
  ;                       :host player1-uri :port player1-port))
  ;    (return-from game-runner nil))
  (if (not (and (player-call/handle player-uris 0 "startGame")
                (player-call/handle player-uris 1 "startGame")))
      (return-from game-runner nil))
  (let ((g (make-instance 'game-state)) (turn 0))
    (start-game g)
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
               (error 'player-error :offender turn :description
                      (with-output-to-string (s) (describe e s)))))

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
           (player-call/handle player-uris (- 1 turn) "opponentPlay" (card-to-struct card-played)
                        play-to-raw draw-from)
           (if (null (deck g)) (return-from outer nil))
           (setf turn (- 1 turn)))))
    (let ((scores (score-game g)))
      (player-call/ignore player-uris 0 "gameEnd" (car scores) (cadr scores))
      (player-call/ignore player-uris 1 "gameEnd" (car scores) (cadr scores))
      (if (> (car scores) (cadr scores)) 0 1))))
