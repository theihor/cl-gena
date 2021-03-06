(load #P"~/quicklisp/setup.lisp")

(ql:quickload 'asdf)

(proclaim '(optimize (debug 3) (safety 3)))
(require 'asdf)

;; (ql:quickload 'fset)
(ql:quickload 'lisp-unit)
(ql:quickload 'cl-quickcheck)
(ql:quickload 'alexandria)
;; (ql:quickload 'cl-graph)
;; (ql:quickload 'cl-heap)
(ql:quickload 'spatial-trees)
(ql:quickload 'spatial-trees.nns)
;; (ql:quickload 'apply-argv)
;;(ql:quickload 'ironclad)
;;(ql:quickload 'babel)
;; (ql:quickload 'cl-svg)
;; (ql:quickload 'yason)
;; (ql:quickload 'cl-geometry)
;; (ql:quickload 'smug)
(ql:quickload 'anaphora)
;; (ql:quickload 'cl-fad)
;; (ql:quickload 'cl-cairo2)
;; (ql:quickload 'cl-containers)
;;(ql:quickload 'trivial-timeout)

(in-package :cl-user)
(asdf:initialize-source-registry '(:source-registry
                                   :inherit-configuration
                                   (:directory :here)
                                   (:directory (:here "src/"))
                                   (:directory (:here "examples/"))))

(asdf:compile-system :cl-gena)
(asdf:load-system :cl-gena)

(asdf:compile-system :cl-gena-examples)
(asdf:load-system :cl-gena-examples)


(print "Welcome.")






