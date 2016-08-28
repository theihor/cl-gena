(defpackage :cl-gena/fancy-tools 
  (:use :common-lisp :anaphora)
  (:export #:compare-number
           #:take-n-max
           #:maximum
           #:appendf)
  )

(in-package :cl-gena/fancy-tools)

(defun compare-number (x y)
  "Comparator used in functions below must return :eq :gt or :lt"
  (cond ((< x y) :lt)
        ((> x y) :gt)
        ((= x y) :eq)
        (t (error "Impossible"))))

(defun maximum (lst &key (comparator #'compare-number) (max nil))
  (if (null lst)
      max
      (let ((x (first lst)))
        (if (and max
                 (eq (funcall comparator x max) :lt))
            (maximum (cdr lst) :max max)
            (maximum (cdr lst) :max x)))))

(defun partition-around-pivot (pivot lst  &key (comparator #'compare-number))
  (let ((ln 0) (mn 0) (rn 0)
        left middle right)
    (loop for x in lst
       for comparison = (funcall comparator x pivot)
       when (eq comparison :lt) do (progn (push x left) (incf ln))
       when (eq comparison :gt) do (progn (push x right) (incf rn))
       when (eq comparison :eq) do (progn (push x middle) (incf mn))) 
    (values left middle right
            ln mn rn)))

(defun take-n-max (n lst &key (comparator #'compare-number))
  "Presumably this function perfoms in linear time
   The order of the result is random
   Second value is the rest of the list"
  (let ((len (length lst)))
    (cond ((>= n len) (values lst nil))
          ((< n 1) (values nil lst))
          (t (multiple-value-bind (left middle right ln mn rn)
                 (partition-around-pivot (nth (random len) lst) lst
                                         :comparator comparator)
               (declare (ignore ln))
               (cond ((= n rn) (values right (append left middle)))
                     ((< n rn) (values (take-n-max n right
                                                   :comparator comparator)
                                       (append left middle)))
                     (t (cond ((= n (+ rn mn)) (values (append middle right) left))
                              ((> n (+ rn mn)) (multiple-value-bind (max-rest rest)
                                                   (take-n-max (- n rn mn) left
                                                               :comparator comparator)
                                                 (values (append max-rest middle right)
                                                         rest)))
                              (t (values (append (subseq middle 0 (- n rn)) right)
                                         (append left (subseq middle (- n rn)))))))))))))

(defmacro appendf (lst &rest lists)
  "(appendf x y) appends y to x, and setf-s the result to x"
  `(setf ,lst
         (funcall #'append ,lst ,@lists)))

