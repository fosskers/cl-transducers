(defsystem "transducers"
  :version "0.1.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license ""
  :depends-on (:sycamore :fset)
  :components ((:module "src"
                :components
                ((:file "transducers"))))
  :description "")
  ;; :in-order-to ((test-op (test-op "transducers/tests"))))

;; (defsystem "transducers/tests"
;;   :author ""
;;   :license ""
;;   :depends-on ("transducers"
;;                "rove")
;;   :components ((:module "tests"
;;                 :components
;;                 ((:file "main"))))
;;   :description "Test system for transducers"
;;   :perform (test-op (op c) (symbol-call :rove :run c)))
