(in-package :cl-user)
(defpackage :expander(:use :cl)
  (:export :expand))
(in-package :expander)

(eval-when(:compile-toplevel :load-toplevel :execute)
  (defmacro prototype(name param-types return-types)
    `(DECLAIM(FTYPE(FUNCTION ,param-types ,return-types),name)))

  (defmacro get-env(&environment env)
    `',env))

(prototype expand(T &optional T)T)
(defun expand(sexp &optional environment)
  (etypecase sexp
    ((AND SYMBOL(NOT(OR BOOLEAN KEYWORD)))
     ; it may symbol-macro, so...
     (expand-symbol-macro sexp environment))
    (ATOM sexp)
    (LIST (let((form(macroexpand sexp environment)))
	    ; macro may be expanded into atom directly, so...
	    (if(atom form)
	      form
	      (typecase (car form)
		((cons (eql lambda) *)
		 `((LAMBDA,(cadar form),@(mapcar #'expand(cddar form)))
		   ,@(cdr form)))
		(list form) ; it may just data.
		(t (%expand form environment))))))))

(prototype expand-symbol-macro((and symbol(not(or null keyword(eql T))))
			       &optional T)T)
(defun expand-symbol-macro(symbol &optional environment)
  (multiple-value-bind(result expandedp)(macroexpand symbol environment)
    (if(not expandedp) ; it is just a symbol, so...
      result ; else symbol-macro may be expanded into atom directly, so...
      (if(atom result)
	result ; else RESULT may include macro form, so...
	(expand result environment)))))

(defvar *expanders* (make-hash-table :test #'equal))

(prototype %expand(cons &optional T)T)
(defun %expand(expanded-form &optional environment)
  (funcall(get-expander(car expanded-form))
    expanded-form environment))

(defun get-expander(key)
  (gethash key *expanders*
	   (lambda(form env)
	     (let((cmf(compiler-macro-function(car form)env)))
	       (if cmf
		 (let((new(funcall *macroexpand-hook* cmf form env)))
		   (if(eq new form)
		     `(,(car form),@(mapcar #'expand (cdr form)))
		     (expand new env)))
		 `(,(car form) ; it may conflicts with symbol-macro.
		    ,@(loop :for form :in(cdr form)
			    :collect(expand form env))))))))

(eval-when(:load-toplevel :compile-toplevel :execute)
  (defmacro defexpander(name lambda-list &body body)
    "define expander.
    syntax: (DEFEXPANDER name lambda-list &BODY body)
    name = (AND SYMBOL (NOT(OR KEYWORD BOOLEAN)))
    lambda-list = (form environment)
    body = S-EXPRESSION*
    FORM shall whole form which first element is NAME.
    ENVIRONMENT shall ENVIRONMENT object."
    `(SETF(GETHASH ',name *EXPANDERS*)
       (LAMBDA ,lambda-list ,@body))))

(prototype copy-expander(#0=(and symbol (not(or boolean keyword)))#0#)
	   (function(cons T)T))
(defun copy-expander(dest src)
  "copy expander function from src to dest"
  (setf (gethash dest *expanders*)
	(the(function(cons T)T)(gethash src *expanders*))))

(defexpander quote(whole env)
  (declare(ignore env))
  whole)

(progn . #.(mapcar(lambda(dest)
		    `(copy-expander ',dest 'quote))
	     '(go declare)))

(defexpander function(whole env)
  (destructuring-bind(op a . b)whole
    (if(symbolp a)
      (if(eq 'lambda (caar b))
	`(,op ,a ,@(funcall(get-expander 'lambda)(car b)env))
	whole)
      (if(eq 'lambda(car a))
	`(,op ,(funcall(get-expander 'lambda)a env),@b)
	whole))))

(defexpander macrolet(whole env)
  (destructuring-bind(op binds . body)whole
    (eval`(,op ,binds (EXPAND(EXPAND ',(if(cdr body) ; BODY has some forms,
					 ; and first form may DECLARE, so...
					 (cons 'locally body)
					 (car body))
				     (GET-ENV))
			,env)))))

(copy-expander 'symbol-macrolet 'macrolet)

(defexpander let(whole env)
  (destructuring-bind(op binds . body)whole
    `(,op,(loop :for elt :in binds
		:if(symbolp elt):collect elt
		:else :collect
		`(,(car elt),(expand(cadr elt)env)))
       ,@(loop :for form :in body :collect (expand form env)))))

(copy-expander 'let* 'let)

(defexpander flet(whole env)
  (destructuring-bind(op binds . body)whole
    `(,op,(loop :for (name params . body):in binds :collect
		`(,name ,(expand-params params env)
			,@(loop :for elt :in body :collect
				(expand elt env))))
       ,@(loop :for form :in body :collect (expand form env)))))

(defun expand-params(params env)
  (loop :for param :in params
	:if (symbolp param) :collect param
	:else :collect
	`(,(car param),(expand(cadr param)env)
	   ,@(when(caddr param)
	       `(,(caddr param))))))

(copy-expander 'labels 'flet)

(defexpander lambda(whole env)
  (destructuring-bind(op params . body)whole
    `(,op ,(expand-params params env)
	  ,@(loop :for form :in body :collect (expand form env)))))

(defexpander the(whole env)
  (destructuring-bind(op type form)whole
    ; TYPE may include AND or OR form, but it is not MACROs.
    `(,op ,type ,(expand form env))))
