(defpackage :cl-gena/random
  (:use :common-lisp)
  (:export #:random-float
           #:random-int
           #:split-at
           #:random-take
           #:probability-check
           #:do-with-probability))

(in-package :cl-gena/random)

(defun random-float (&key (from 0.0) (to 1.0))
  (when (> from to) (error "Invalid bounds"))
  (+ from (random (- to from))))

(defun random-int (&key (from 1) (to 100))
  (when (> from to) (error "Invalid bounds"))
  (+ from (random (1+ (- to from)))))

(defun split-at (n lst &key acc)
  "Returns (values nth-element left-list right-list)"
  (let ((len (length lst)))
    (cond ((= n 0) (values (car lst) (reverse acc) (cdr lst)))
          ((>= n len) (values nil lst nil))
          ((< n 0) (values nil nil lst)) 
          (t (split-at (1- n) (cdr lst) :acc (cons (car lst) acc))))))

(defun random-take (n lst)
  (let ((len (length lst)))
    (cond ((= n 1) (list (nth (random len) lst)))
          ((< n 1) nil)
          ((> n len) lst)
          (t (multiple-value-bind (x l r)
                 (split-at (random len) lst)
               (cons x (random-take (1- n) (append l r))))))))

(defun probability-check (p)
  (cond ((= p 1.0) t)
        ((= p 0.0) nil)
        ((or (< p 0.0) (> p 1.0)) (error "Probability must be in range [0..1]"))
        (t (< (random 1.0) p))))

(defmacro do-with-probability (p &body body)
  `(when (probabilty-check ,p)
     ,@body))

