(defpackage :cl-gena-examples/knapsack
  (:nicknames :knapsack-example)
  (:use :common-lisp :anaphora :cl-gena/bit-array-genotype :cl-gena/generic-defs :cl-gena/random))

(in-package :knapsack-example)

;; In this example we are using cl-gena to solve the knapsack problem
;; https://en.wikipedia.org/wiki/Knapsack_problem

;; our input is the set of items we can put into the knapsack
;; each item has value and weight
(defparameter *items*
  (list (list :value 1 :weight 1)
        (list :value 2 :weight 1)
        (list :value 3 :weight 1)
        (list :value 1 :weight 2)
        (list :value 2 :weight 2)
        (list :value 3 :weight 2)
        (list :value 1 :weight 3)
        (list :value 2 :weight 3)
        (list :value 3 :weight 3)))

;; let's convert this to hash-table to improve performance
(defun plists-to-hash-table (plists)
  (let ((table (make-hash-table :test #'eq)))
    (loop
       for plist in plists
       for i = 0 then (1+ i)  
       do (setf (gethash i table) plist))
    table))

(setf *items* (plists-to-hash-table *items*))

(defun items-generator (count &key (min-val 1) (max-val 100) (min-w 1) (max-w 100))
  (loop repeat count collect
       (list :value (random-int :from min-val :to max-val)
             :weight (random-int :from min-w :to max-w))))

;; and knapsack size
(defparameter *knapsack-size* 13)

;; Every genotype (e. g. individual) represents particular solution
;; which is the set of items we put in the knapsack
;; we can represent it as array of bits, where each index is an item
;; therefore we can use bit-array-genotype class to represent a solution
(defclass knapsack-genotype (bit-array-genotype) ())
(defclass knapsack-population (bag-population) ())

;; the fitness must tell how good is particular solution
;; in this problem we want to maximize the value of knapsack
(defmethod fitness ((g knapsack-genotype))
  (let* ((s (get-sequence g))
         (w 0)
         (v 0))
    (loop
       for i = 0 then (1+ i)
       while (< i (size g))
       when (= 1 (bit s i))
       do (progn (incf w (getf (gethash i *items*) :weight))
                 (incf v (getf (gethash i *items*) :value))))
    (if (> w *knapsack-size*)
        (- *knapsack-size* w)
        v)))

;; let's make our own genotype generator to avoid generating a lot of invalid solutions
(defun knapsack-genotype-generator (size)
  (lambda (index)
    (declare (ignore index))
    (let ((w-sum 0))
      (make-instance 'knapsack-genotype
                     :sequence (loop with array = (make-array size :element-type 'bit) 
                                  do (let* ((i (random size))
                                            (w (getf (gethash i *items*) :weight)))
                                       (if (> (+ w w-sum) *knapsack-size*)
                                           (return array)
                                           (progn (setf (bit array i) 1)
                                                  (incf w-sum w)))))
                     :size size))))

(defun solve-knapsack (&key genotype-size population-size timeout max-iteration)
  (let ((*initial-mutation-width* 0.2)
        (*mutation-decrease-rate* :linear)
        (*elitism-rate* 0.15)
        (*tournament-rate* 0.3)) 
    (let* ((pop (initialize-population
                 knapsack-population
                 (knapsack-genotype-generator genotype-size)
                 population-size))
           (the-best (fitness (best-genotype pop)))) 
      (evolution pop
                 :timeout timeout
                 :max-iteration max-iteration
                 :step-func (lambda (population i elapsed-time)
                              (let* ((best (best-genotype population))
                                     (value (fitness best)))
                                (when (< the-best value)
                                  (setf the-best value))
                                (format t "~A [~A s]: ~A~%"
                                        i (float elapsed-time) the-best))))
      (format t "BEST VALUE = ~A~%" the-best))))

(defun brute-force-solver (items knapsack-size)
  (let* ((n (hash-table-count items)) 
         (best-v 0)
         (start-time (cl:get-internal-real-time))
         (current-time start-time)
         )
    (labels ((%inc (lst)
               (when lst
                 (let ((i (1+ (car lst))))
                   (if (< i n)
                       (cons i (cdr lst))
                       (if (cdr lst)
                           (loop for tail = (%inc (cdr lst)) then (%inc tail)
                              when (not (= (1- i) (car tail))) return (cons (1+ (car tail)) tail))
                           (list i)))))))
      (loop for k from 1 to (1- n) do 
           (let ((indexes (reverse (loop for i from 0 below k collect i)))
                 (last-indexes (reverse (loop for i from (- n k) to (1- n) collect i)))) 
             (loop do (let ((w 0) (v 0)) 
                        (loop for i in indexes do
                             (let ((w1 (getf (gethash i items) :weight)))
                               (if (> (+ w w1) knapsack-size)
                                   (return)
                                   (progn (incf w w1)
                                          (incf v (getf (gethash i items) :value))))))
                        (when (> v best-v)
                          (setf best-v v))
                        (setf indexes (%inc indexes)))
                until (equal indexes last-indexes) )
             (setf current-time (cl:get-internal-real-time))
             (format t "[~A s] BEST VALUE ON K = ~A: ~A~%"
                     (float (/ (- current-time start-time)
                          cl:internal-time-units-per-second))
                     k best-v))))))

(defun run (items knapsack-size &key (population-size 100) (timeout 10.0)) 
  (let ((*items* items)
        (*knapsack-size* knapsack-size))
    (solve-knapsack :genotype-size (hash-table-count *items*)
                    :population-size population-size
                    :timeout timeout
                    )))


