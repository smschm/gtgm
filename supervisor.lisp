(defpackage :gt-super
  (:use :common-lisp :gt-game :s-xml-rpc :bordeaux-threads))
(in-package :gt-super)

(defmacro player-call (player-num &rest call)
  `(xml-rpc-call (encode-xml-rpc-call ,@call)
                 :host (car (elt player-uris ,player-num))
                 :port (cdr (elt player-uris ,player-num))))

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
  (if (not (and (player-call 0 "startGame") (player-call 1 "startGame")))
      (return-from game-runner nil))
  (let ((g (make-instance 'game-state)) (turn 0))
    (start-game g)
    (player-call 0 "initialize" 0 0 ; opponent id = game id = 0 for now
                 0 (mapcar #'card-to-struct (elt (hands g) 0)))
    (player-call 1 "initialize" 0 0 ; opponent id = game id = 0 for now
                 1 (mapcar #'card-to-struct (elt (hands g) 1)))
    (block outer (loop do
         (let* ((r0 (player-call
		     turn "getPlay"
		     (cardlist-to-xml (elt (hands g) turn))
		     (cardlistvector-to-xml (discards g))
		     (cardlistmatrix-to-xml (expos g))
		     (length (deck g))))
		;(dummy (describe r0))
                (card-ix (get-xml-rpc-struct-member r0 :|card_ix|))
                (play-to (get-xml-rpc-struct-member r0 :|play_to|))
                (draw-from (get-xml-rpc-struct-member r0 :|draw_from|))
                (card-played (elt (elt (hands g) turn) card-ix)))
	   (princ (list card-ix play-to draw-from))
	   (describe g)
	   (if (equalp :discarded-instead
		       (place-card g turn card-ix
				   (if (= play-to 0) :discard :expo)))
	       (setf play-to 0))
           (if (equalp :drew-from-deck
		       (player-draw-card g turn
					 (if (< draw-from 0) :deck :discard)
					 :pile-ix draw-from))
	       (setf draw-from -1))
           (if (null (deck g)) (return-from outer nil))
           ;(player-call turn "drawnCard" (mapcar #'card-to-struct (elt (hands g) turn)))
           (player-call (- 1 turn) "opponentPlay" (card-to-struct card-played)
                        play-to draw-from)
           (setf turn (- 1 turn)))))
    (let ((scores (score-game g)))
      (player-call 0 "gameEnd" (car scores) (cadr scores))
      (player-call 1 "gameEnd" (car scores) (cadr scores)))))
                               
                             
