(in-package :cl-user)
(defpackage :expander(:use :cl)
  (:export :expand))
(in-package :expander)

#+ecl ; environment is special, so we can not support it.
      ; but without macrolet which has &whole and &environment,
      ; EXPEND works fine. so...
(warn "EXPAND does not support ~A."(lisp-implementation-type))

(eval-when(:compile-toplevel :load-toplevel :execute)
  (defmacro prototype(name param-types return-types)
    `(declaim(ftype(function ,param-types ,return-types),name)))

  (defmacro get-env(&environment env)
    env))

(prototype expand(T &optional list)T)
(defun expand(sexp &optional environment)
  "Expand all of macro, macrolet, symbol-macro and symbol-macrolet."
  (etypecase sexp
    ((or null keyword) sexp)
    (symbol ; it may symbol-macro, so...
      (expand-symbol-macro sexp environment))
    (atom sexp)
    (list
      (case(car sexp)
	#+clisp ; signals an error when expand it out of loop, so...
	(loop-finish '#.(loop do(return(macroexpand'(loop-finish)(get-env)))))
	(otherwise
	  (let((form(macroexpand sexp)))
	    ; macro may be expanded into atom directly, so...
	    (if(atom form)
	      form
	      (%expand form environment))))))))

(prototype expand-symbol-macro((and symbol(not null keyword))&optional list)T)
(defun expand-symbol-macro(symbol &optional environment)
  (multiple-value-bind(expanded findp)(get-local-expander symbol environment)
    (if findp ; EXPANDED form may include macro form, so...
      (expand expanded environment)
      (multiple-value-bind(result expandedp)(macroexpand symbol)
	(if(not expandedp)
	  result ; else symbol-macro may be expanded into atom directly, so...
	  (if(atom result)
	    result ; else RESULT may include macro form, so...
	    (expand result environment)))))))

(prototype %expand((and list(not null))list)T)
(defun %expand(expanded-form environment)
  (case(car expanded-form)
    ((quote function) expanded-form) ; ignore
    ((macrolet)
     (expand(expand-macrolet expanded-form)))
    ((symbol-macrolet)
     `(progn ,@(loop for form in (cddr expanded-form) ; do only its body.
		     collect(expand form(add-local-expanders(second expanded-form)environment)))))
    ((labels flet)
     (destructuring-bind(op binds . body)expanded-form
       `(,op,(loop for (name params . body) in binds collect
		   `(,name ,params ,@(loop for elt in body collect
					   (expand elt environment))))
	  ,@body)))
    ((lambda)
     (destructuring-bind(lambda params . body)expanded-form
       `(,lambda ,params ,@(loop for elt in body collect
				 (expand elt environment)))))
    (otherwise
      `(,(car expanded-form) ; it may conflicts with symbol-macro.
	 ,@(loop for form in (cdr expanded-form)
		 collect(expand form environment))))))

(prototype add-local-expanders(list list) list)
(defun add-local-expanders(binds environment)
  (if(endp binds)
    environment
    (add-local-expanders (cdr binds)
			 (destructuring-bind(name body)(car binds)
			   (acons name body environment)))))

(prototype expand-mecrolet((cons(eql macrolet)(cons list t)))
	   (cons(eql progn)list))
(defun expand-macrolet(form)
  (destructuring-bind(macrolet binds . body)form
    (multiple-value-bind
      (env expanders)(eval`(,macrolet,(subst '&rest '&body binds)
			     (let((env(get-env)))
			       (values env
				       (loop for name in ',(mapcar #'car binds)
					     collect
					     (cons name
						   (macro-function name env)))))))
      (labels((rec(sexps)
		(loop for sexp in sexps collect
		      (if(atom sexp)
			sexp
			(let((op(car sexp)))
			  (if(find op '(quote function))
			    sexp
			    (let((expander(assoc op expanders)))
			      (if expander
				(funcall(cdr expander)sexp env)
				(rec sexp)))))))))
	`(progn ,@(rec body))))))

(prototype get-local-expander((and symbol(not null keyword))list)
	   (values list boolean))
(defun get-local-expander(symbol environment)
  (let((expander(assoc symbol environment)))
    (if expander
      (values(cdr expander)T)
      (values nil nil))))
