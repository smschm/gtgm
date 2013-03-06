(defpackage :elo
  (:use :common-lisp)
  (:export :new-rating))

(in-package :elo)

(defparameter *k* 5.0d0)
(defparameter *exp-base*
  (/ (log 10) 400))

(defun translate (wonp)
  (if (equal wonp :tie) 0.5
      (if wonp 1 0)))

(defun new-rating (r-a r-b a-won-p)
  (let* ((q-a (exp (* *exp-base* r-a)))
         (q-b (exp (* *exp-base* r-b)))
         (e-a (/ q-a (+ q-a q-b)))
         (e-b (/ q-b (+ q-a q-b)))
         (s-a (translate a-won-p))
         (s-b (- 1 s-a)))
    (cons (+ r-a (* *k* (- s-a e-a)))
          (+ r-b (* *k* (- s-b e-b))))))
