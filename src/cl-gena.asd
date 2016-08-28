(asdf:defsystem :cl-gena
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :pathname #p"./"
  :depends-on (:cl-gena/generic-defs :cl-gena/vector-genotype :cl-gena/random :cl-gena/fancy-tools)
  :in-order-to ((test-op ;; (load-op :src/test/field)
                         ))
  :perform (test-op (o c)
                    ;; (lisp-unit:run-tests :all :src/test/field)
                    ))

(register-system-packages :spatial-trees '(:rectangles))

