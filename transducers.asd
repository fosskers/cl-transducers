(defsystem "transducers"
  :version "0.1.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license ""
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
  :author ""
  :license ""
  :depends-on (:transducers :parachute)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for transducers"
  :perform (test-op (op c) (symbol-call :parachute :test :transducers/tests)))

(defsystem "transducers-csv"
  :version "0.1.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license ""
  :depends-on (:transducers :str)
  :components ((:module "csv"
                :components
                ((:file "csv"))))
  :description "CSV extension for Transducers.")

(defsystem "transducers-jzon"
  :version "0.1.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license ""
  :depends-on (:transducers :com.inuoe.jzon)
  :components ((:module "jzon"
                :components
                ((:file "jzon"))))
  :description "JSON extension for Transducers.")
