(asdf:load-system :transducers)
(asdf:load-system :transducers/tests)
(in-package :transducers/tests)

(let ((status (parachute:status (parachute:test 'transducers/tests))))
  (cond ((eq :PASSED status) (uiop:quit))
        (t (uiop:quit 1))))
