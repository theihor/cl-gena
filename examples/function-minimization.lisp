(defpackage :cl-gena-examples/function-minimization
  (:nicknames :fm-example)
  (:use :common-lisp :cl-gena/vector-genotype :cl-gena/generic-defs))

(in-package :fm-example)

;; In this example we are using cl-gena to find optimum
;; for some mathematical functions of several variables of kind
;;     f(x1, x2, ... , xN)

;; Every genotype (e. g. individual) represents particular solution
;; which is a vector of function parameters X = #(x1 x2 ... xN)
;; therefore we can use pure vector-genotype class to represent them
(defclass fm-genotype (vector-genotype) ())
(defclass fm-population (vg-population) ())

;; Let's take the hypersphere of N variables, the sum of squares
;;   f(X) = x1^2 + x2^2 + ... + xN^2
;; it has known global minimum at f(X = 0) = 0, so is good for testing
(defun hypersphere (v)
  (loop for x across v sum (* x x)))

;; Next we should define a fitness function, which will tell us how good is particular solution (genotype)
;; so, the closer hypersphere(g) is to 0, the higher must be the value of fitness(g)
(defmethod fitness ((g fm-genotype))
  (/ 1.0 (+ 1.0 (hypersphere (get-sequence g)))))

(defun minimize-hypersphere (&key genotype-size population-size lower-bound upper-bound timeout max-iteration)
  (let ((*initial-mutation-depth* 1e-4)
        (*initial-mutation-width* 0.2)
        (*mutation-decrease-rate* :linear)
        (*elitism-rate* 0.15)
        (*tournament-rate* 0.6)) 
    (let* ((pop (initialize-population
                 fm-population
                 (vector-genotype-generator genotype-size lower-bound upper-bound :type 'fm-genotype)
                 population-size))
           (the-best (hypersphere (get-sequence (best-genotype pop))))) 
      (evolution pop
                 :timeout timeout
                 :max-iteration max-iteration
                 :step-func (lambda (population i elapsed-time)
                              (let* ((best (best-genotype population))
                                     (minimum (hypersphere (get-sequence best))))
                                (when (> the-best minimum)
                                  (setf the-best minimum))
                                (format t "~A [~A s]: ~1,4E~%"
                                        i (float elapsed-time) the-best))))
      (format t "FOUND MINIMUM = ~1,2E~%" the-best))))

;; this configuration should get around 0.5e-8 minimum
(defun run ()
  (minimize-hypersphere
   :genotype-size 12
   :population-size 150
   :lower-bound -5.0
   :upper-bound +5.0
   :timeout 10.0
   :max-iteration 200))
