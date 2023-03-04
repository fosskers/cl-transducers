(defsystem "transducers"
  :version "0.1.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "AGPL-3.0-only"
  :depends-on (:sycamore :fset)
  :components ((:module "transducers"
                :components
                ((:file "transducers")
                 (:file "reducers")
                 (:file "generators")
                 (:file "entry")
                 (:file "utils"))))
  :description "Ergonomic, efficient data processing."
  :in-order-to ((test-op (test-op :transducers/tests))))

(defsystem "transducers/tests"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "AGPL-3.0-only"
  :depends-on (:transducers :transducers-jzon :parachute)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for transducers"
  :perform (test-op (op c) (symbol-call :parachute :test :transducers/tests)))

(defsystem "transducers-jzon"
  :version "0.1.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "AGPL-3.0-only"
  :depends-on (:transducers :com.inuoe.jzon :trivia)
  :components ((:module "jzon"
                :components
                ((:file "jzon"))))
  :description "JSON extension for Transducers.")
