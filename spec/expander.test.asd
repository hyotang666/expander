; vim: ft=lisp et
(in-package :asdf)
(defsystem :expander.test
  :version "0.0.1"
  :depends-on
  (:jingoh "expander")
  :components
  ((:file "expander"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :expander args)))
