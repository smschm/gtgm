(defpackage :gt-game
  (:use :common-lisp :s-xml-rpc)
  (:export :game-state :start-game :place-card :player-draw-card :score-game
           :deck :discards :hands :expos))
(in-package :gt-game)

(defparameter +ranks+ (append '(0 0 0) (loop for i from 2 below 11 collect i)))
(defparameter +suits+ 5)
(defparameter +init-hand-size+ 8)

(defun make-adj-vector-from (list)
  (make-array (length list) :initial-contents list :adjustable t))

(defun shuffle (list)
  (if (< (length list) 2) list
      (let ((item (elt list (random (length list)))))
	(cons item (shuffle (delete item list))))))

(defun make-fresh-deck ()
  (apply #'append
	 (loop for suit below 5 collect
	      (loop for rank in +ranks+
		 collect (cons suit rank)))))

(defun score-pile-v (pile)
  (let ((penalty 0) (multiplier 1) (total 0))
    (loop for c across pile do
	 (progn (setf penalty -20)
		(if (= (cdr c) 0) (incf multiplier)
		    (incf total (cdr c)))))
    (* multiplier (+ penalty total))))

(defun score-pile (pile &key (penalized nil) (total 0) (multiplier 1))
  (if (null pile) (* (+ total (if penalized -20 0)) multiplier)
      (score-pile (cdr pile)
		  :penalized t
		  :total (if (> (cdar pile) 1) (+ total (cdar pile)) total)
		  :multiplier (if (= (cdar pile) 0)
				  (+ multiplier 1) multiplier))))

;(defclass game-state ()
;  ((started :initform nil :accessor started)
;   (hands :initform (make-array 2 :initial-contents
;			(loop for i below 2 collect
;			     (make-array 0 :fill-pointer 0)))
;	  :accessor hands)
;   (deck :initform (shuffle (make-fresh-deck)) :accessor deck)
;   (discards :initform (make-array +suits+ :initial-contents
;			   (loop for i below +suits+ collect
;				(make-array 0 :fill-pointer 0)))
;	     :accessor discards)
;   (trails :initform (make-array 2 :initial-contents
;			 (loop for i below 2 collect
;			      (make-array +suits+ :initial-contents
;				    (loop for j below +suits+ collect
;					 (make-array 0 :fill-pointer 0)))))
;	   :accessor trails)))

(defclass game-state ()
  ((started :initform nil :accessor started)
   (hands :initform (make-array 2 :initial-element nil)
	  :accessor hands)
   (deck :initform (shuffle (make-fresh-deck)) :accessor deck)
   (discards :initform (make-array +suits+ :initial-element nil)
	     :accessor discards)
   (expos :initform (make-array '(2 5) :initial-element nil)
	   :accessor expos)))

(defmethod start-game ((g game-state))
  (setf (started g) t
;	(hands g) (make-array 2 :adjustable t :initial-contents
;		     (loop for i below 2 collect
;			  (make-adj-vector-from
;			   (subseq (deck g) (* i +init-hand-size+)
;				   (* (1+ i) +init-hand-size+)))))
	(hands g) (make-array 2 :initial-contents
		     (loop for i below 2 collect
			  (subseq (deck g) (* i +init-hand-size+)
				   (* (1+ i) +init-hand-size+))))
	(deck g) (subseq (deck g) (* 2 +init-hand-size+))))

(defmethod expedition-max ((g game-state) player suit)
  (let ((expn (aref (expos g) player suit)))
    (if (null expn) 0
	(cdar expn))))

(defmethod place-card ((g game-state) player card-ix where)
  (let* ((player-hand (elt (hands g) player))
	 (card (elt player-hand card-ix))
	 (suit (car card))
	 (rank (cdr card))
	 (remaining-cards (append (subseq player-hand 0 card-ix)
				  (subseq player-hand (1+ card-ix))))
	 (card-placed t)
	 (status nil))
    (case where
      ((:discard)
       (progn (setf (elt (discards g) suit)
		    (cons card (elt (discards g) suit)))
	      ; why doesn't push work here?
	      (setf status t)))
      ((:expo)
       (if (< rank (expedition-max g player suit))
	   (progn (place-card g player card-ix :discard)
		  (setf card-placed nil status :discarded-instead))
	   (progn (push card (aref (expos g) player suit))
		  (setf status t))))
      (otherwise (setf card-placed nil)))
    (if card-placed (setf (elt (hands g) player) remaining-cards))
    status))

(defmethod player-draw-card ((g game-state) player where &key pile-ix)
  (let ((status nil))
    (case where
      ((:discard)
       (if (null (elt (discards g) pile-ix))
	   (progn (player-draw-card g player :deck)
		  (setf status :drew-from-deck))
	   (let ((card (pop (elt (discards g) pile-ix))))
	     (push (elt (hands g) player) card)
	     (setf status t))))
      ((:deck)
       (let ((card (pop (deck g))))
	 (format t "pushing card ~A onto ~A~%" card (elt (hands g) player))
	 ;(push (elt (hands g) player) card)
	 (setf (elt (hands g) player)
	       (cons card (elt (hands g) player)))
	 (setf status t))))
    status))

(defmethod score-game ((g game-state))
  (loop for i below 2 collect
       (loop for j below +suits+ summing (score-pile (aref (expos g) i j)))))
