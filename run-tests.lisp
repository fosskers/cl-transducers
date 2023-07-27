(ql:quickload :transducers)
(ql:quickload :transducers/tests)
(in-package :transducers/tests)

(let ((status (parachute:status (parachute:test 'transducers/tests))))
  (cond ((eq :PASSED status) (uiop:quit))
        (t (uiop:quit 1))))
