; vim: ft=lisp et
(in-package :asdf)
(defsystem :expander
  :version "0.0.1"
  :depends-on
  (
   "introspect-environment"     ; Wrapper for environment introspection.
   "alexandria"                 ; Public domain utilities.
   "lambda-fiddle"              ; Utilities for lambda list parsing.
   "trestrul"                   ; Utilities for tree structured list.
   )
  :components((:file "expand")))

;; These two methods below are added by JINGOH.GENERATOR.
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "expander"))))
  (append (call-next-method) '((test-op "expander.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "expander"))) &rest keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
(defmethod operate :around(o (c (eql (find-system "expander")))
                             &key((:compile-print *compile-print*))
                             ((:compile-verbose *compile-verbose*))
                             &allow-other-keys)
  (call-next-method))
