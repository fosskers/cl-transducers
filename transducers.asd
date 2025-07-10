(defsystem "transducers"
  :version "1.4.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :depends-on ()
  :serial t
  :components ((:module "transducers"
                :components
                ((:file "package")
                 (:file "deprecated")
                 (:file "utils")
                 (:file "transducers")
                 (:file "reducers")
                 (:file "sources")
                 (:file "entry")
                 (:file "conditions"))))
  :description "Ergonomic, efficient data processing."
  :in-order-to ((test-op (test-op :transducers/tests))))

(defsystem "transducers/jzon"
  :version "1.4.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :depends-on (:transducers :com.inuoe.jzon :trivia)
  :components ((:module "jzon" :components ((:file "jzon"))))
  :description "JSON extension for Transducers.")

(defsystem "transducers/fset"
  :version "1.4.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :depends-on (:transducers :fset)
  :components ((:module "fset" :components ((:file "fset"))))
  :description "Fset extension for Transducers.")

(defsystem "transducers/tests"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :depends-on (:transducers
               :transducers/jzon
               :transducers/fset
               :fset
               :parachute
               :str)
  :components ((:module "tests" :components ((:file "tests"))))
  :description "Test system for transducers"
  :perform (test-op (op c) (symbol-call :parachute :test :transducers/tests)))
