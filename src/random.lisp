(defpackage :cl-gena/random
  (:use :common-lisp)
  (:export #:random-float
           #:split-at
           #:random-take))

(in-package :cl-gena/random)

(defun random-float (&key (from 0.0) (to 1.0))
  (when (> from to) (error "Invalid bounds"))
  (+ from (random (- to from))))

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