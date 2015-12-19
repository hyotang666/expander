(in-package :cl-user)
(defpackage :expander.asd(:use :cl :asdf))
(in-package :expander.asd)
(defsystem :expander
  :components((:file "expand")))
