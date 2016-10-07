(asdf:defsystem :cl-gena-examples
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :pathname #p"./"
  :depends-on (:cl-gena :cl-gena-examples/function-minimization)
  :in-order-to ((test-op ;; (load-op :src/test/field)
                         ))
  :perform (test-op (o c)
                    ;; (lisp-unit:run-tests :all :src/test/field)
                    ))


