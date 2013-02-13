(defun shuffle (list)
  (if (< (length list) 2) list
      (let ((item (elt list (random (length list)))))
	(cons item (shuffle (delete item list))))))

(defun make-fresh-deck ()
  (apply #'append
	 (loop for suit below 5 collect
	      (loop for rank in
		   (append '(0 0 0) (loop for i from 2 below 11 collect i))
		 collect (cons suit rank)))))

(defun score-pile (pile &key (penalized nil) (total 0) (multiplier 1))
  (if (null pile) (* (+ total (if penalized -20 0)) multiplier)
      (score-pile (cdr pile)
		  :penalized t
		  :total (if (> (cdar pile) 1) (+ total (cdar pile)) total)
		  :multiplier (if (= (cdar pile) 0)
				  (+ multiplier 1) multiplier))))

(defclass game-state ()
  ((started :initform nil :accessor started)
   (hands :initform '(nil nil) :accessor hands)
   (deck :initform (shuffle (make-fresh-deck)) :accessor deck)
   (discards :initform (make-array 5 :initial-element nil) :accessor discards)
   (trails :initform (list (make-array 5 :initial-element nil)
			   (make-array 5 :initial-element nil))
	   :accessor trails)))

(defmethod start-game ((g game-state))
  (setf (started g) t
	(hands g) (list (subseq (deck g) 0 8)
			(subseq (deck g) 8 16))
	(deck g) (subseq (deck g) 16)))

(defmethod expedition-max ((g game-state) player suit)
  (let ((expn (elt (elt (trails g) player) suit)))
    (if (null expn) 0
	(cdar expn))))

(defmethod place-card ((g game-state) player card-ix where)
  (let* ((player-hand (elt (hands g) player))
	 (card (elt player-hand card-ix))
	 (suit (car card))
	 ;(rank (cdr card))
	 (remaining-cards (append (subseq player-hand 0 card-ix)
				  (subseq player-hand (1+ card-ix))))
	 (card-placed t)
	 (status nil))
    (case where
      ((discard-pile)
       (progn (push card (elt (discards g) suit))
	      (setf status t)))
      ((expedition)
       (if (< (cdr card) (expedition-max g player suit))
	   (progn (place-card g player card-ix 'discard-pile)
		  (setf card-placed nil status 'discarded-instead))
	   (progn (push card (elt (elt (trails g) player) suit))
		  (setf status t))))
      (otherwise (setf card-placed nil)))
    (if card-placed (setf (elt (hands g) player) remaining-cards))
    status))

(defmethod player-draw-card ((g game-state) player where &key pile-ix)
  (let ((status nil))
    (case where
      ((discard-pile)
       (if (null (elt (discards g) pile-ix))
	   (progn (player-draw-card g player 'deck)
		  (setf status 'drew-from-deck))
	   (let ((card (pop (elt (discards g) pile-ix))))
	     (push (elt (hands g) player) card)
	     (setf status t))))
      ((deck)
       (let ((card (pop (deck g))))
	 (push (elt (hands g) player) card)
	 (setf status t))))
    status))
