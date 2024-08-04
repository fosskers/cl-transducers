(defsystem "transducers"
  :version "1.2.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "LGPL-3.0-only"
  :depends-on (:sycamore)
  :components ((:module "transducers"
                :components
                ((:file "transducers")
                 (:file "reducers")
                 (:file "sources")
                 (:file "iterator")
                 (:file "entry")
                 (:file "conditions")
                 (:file "utils"))))
  :description "Ergonomic, efficient data processing."
  :in-order-to ((test-op (test-op :transducers/tests))))

(defsystem "transducers/tests"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "LGPL-3.0-only"
  :depends-on (:transducers
               :transducers/jzon
               :transducers/fset
               :fset
               :parachute
               :str)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for transducers"
  :perform (test-op (op c) (symbol-call :parachute :test :transducers/tests)))

(defsystem "transducers/jzon"
  :version "1.2.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "LGPL-3.0-only"
  :depends-on (:transducers :com.inuoe.jzon :trivia)
  :components ((:module "jzon"
                :components
                ((:file "jzon"))))
  :description "JSON extension for Transducers.")

(defsystem "transducers/fset"
  :version "1.2.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "LGPL-3.0-only"
  :depends-on (:transducers :fset)
  :components ((:module "fset"
                :components
                ((:file "fset"))))
  :description "Fset extension for Transducers.")
