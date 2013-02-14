(defpackage :gt-super
  (use :common-lisp :gt-game :s-xml-rpc :bordeaux-threads))

(defmacro player-call (player-num &rest call)
  `(xml-rpc-call (encode-xml-rpc-call ,@call)
                 :host (intern (concatenate 'string "PLAYER" (format nil "~d" ,player-num) "-URI"))
                 :port (intern (concatenate 'string "PLAYER" (format nil "~d" ,player-num) "-PORT"))))

(defmacro with-xml-struct-bindings (v vars &rest body)
  `(let ,(loop for b in vars collect `(,b (get-xml-struct-member ,v ,b)))
     ,@body))

(defun card-to-struct (c) (xml-rpc-struct :rank (cdr c) :suit (car c)))

(defun game-runner (player0-uri player0-port player1-uri player1-port)
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
    (player-call 0 "initialize" 0 0 ; opponent id = game id = 0 for now
                 1 (mapcar #'card-to-struct (elt (hands g) 1)))
    (block outer (loop do
         (let* ((r0 (player-call turn "getPlay"))
                (card-ix (get-xml-struct-member r0 card_ix)) ; TODO: replace with macro above
                (play-to (get-xml-struct-member r0 play_to))
                (draw-from (get-xml-struct-member r0 draw_from))
                (card-played (elt (elt (hands g) turn) card-ix)))
           (place-card g turn card-ix (if (= play-to 0) 'discard-pile 'expedition))
           (player-draw-card g turn (if (< draw-from 0) 'deck 'discard-pile)
                             :pile draw-from)
           (if (null (deck g)) (return-from outer nil))
           (player-call turn "drawnCard" (mapcar #'card-to-struct (elt (hands g) turn)))
           (player-call (- 1 turn) "opponentPlay" (card-to-struct card-played)
                        play-to draw-from)
           (setf turn (- 1 turn)))))
    (let ((scores (score-game g)))
      (player-call 0 "gameEnd" (car g) (cadr g))
      (player-call 1 "gameEnd" (car g) (cadr g)))))
                               
                             
