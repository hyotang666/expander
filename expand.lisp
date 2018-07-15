(in-package :cl-user)
(defpackage :expander(:use :cl)
  (:export :expand))
(in-package :expander)

(eval-when(:compile-toplevel :load-toplevel :execute)
  (defmacro prototype(name param-types return-types)
    `(DECLAIM(FTYPE(FUNCTION ,param-types ,return-types),name)))

  (defmacro get-env(&environment env)
    `',env))

;;;; EXPAND
(prototype expand(T &optional T)T)
(defun expand(form &optional environment)
  (etypecase form
    ((AND SYMBOL(NOT(OR BOOLEAN KEYWORD)))
     ;; it may symbol-macro, so...
     (expand-symbol-macro form environment))
    (ATOM form)
    (LIST (typecase (car form)
	    ((cons (eql lambda) *)
	     `((LAMBDA,(cadar form),@(mapcar (lambda(sub-form)
					       (expand sub-form environment))
					     (cddar form)))
	       ,@(cdr form)))
	    (list form) ; it may just data.
	    (t (%expand form environment))))))

(prototype expand-symbol-macro((AND SYMBOL (NOT (OR KEYWORD BOOLEAN)))
			       &optional T)T)
(defun expand-symbol-macro(form &optional environment)
  (call-with-macroexpand-check form environment #'%expand))

;; MACROEXPAND%
(defun macroexpand%(form env)
  (multiple-value-bind(new expanded?)(macroexpand-1 form env)
    (if expanded?
      (if (eq form new) ; &whole works.
	new
	(macroexpand% new env))
      new)))

;;; %EXPAND
(prototype %expand(cons &optional T)T)
(defun %expand(form &optional environment)
  (funcall (get-expander(car form))
	   form environment))

;;; *SPECIAL-FORM-EXPANDERS*
(defvar *special-form-expanders* (make-hash-table :test #'equal))

;; CALL-WITH-MACROEXPAND-CHECK
(defun call-with-macroexpand-check(form env cont)
  (let((result (macroexpand% form env)))
    (if(atom result) ; may be expanded into atom directly.
      result ; else RESULT  may include macro form in its sub-forms.
      (funcall (gethash (car result) *special-form-expanders* cont)
	       result env))))

(defun get-expander(key)
  (gethash key *special-form-expanders*
	   ;; As default expander. Special forms are never comes.
	   (lambda(form env)
	     (let((cmf(compiler-macro-function(car form)env)))
	       (if(null cmf)
		 (call-with-macroexpand-check form env #'expand-sub-form)
		 (let((new(funcall *macroexpand-hook* cmf form env)))
		   (if(not(eq new form))
		     (expand new env)
		     ;; else compiler macro may be defined on macro.
		     (if(macro-function(car form))
		       (expand(macroexpand% form env)env)
		       (expand-sub-form form env)))))))))

(defun expand-sub-form(form env)
  `(,(car form)
     ,@(mapcar (lambda(sub-form)
		 (expand sub-form env))
	       (cdr form))))

;;;; DSL
(eval-when(:load-toplevel :compile-toplevel :execute)
  (defmacro defexpander(name lambda-list &body body)
    "define expander.
    syntax: (DEFEXPANDER name lambda-list &BODY body)
    name = (AND SYMBOL (NOT(OR KEYWORD BOOLEAN)))
    lambda-list = (form environment)
    body = S-EXPRESSION*
    FORM shall whole form which first element is NAME.
    ENVIRONMENT shall ENVIRONMENT object."
    `(SETF(GETHASH ',name *SPECIAL-FORM-EXPANDERS*)
       (LAMBDA ,lambda-list ,@body))))

(prototype copy-expander(#0=(and symbol (not(or boolean keyword)))#0#)
	   (function(cons T)T))
(defun copy-expander(dest src)
  "copy expander function from src to dest"
  (setf (gethash dest *special-form-expanders*)
	(the(function(cons T)T)(gethash src *special-form-expanders*))))

;;;; DEFINITIONS
(defexpander quote(whole env)
  (declare(ignore env))
  whole)

(progn . #.(mapcar(lambda(dest)
		    `(copy-expander ',dest 'quote))
	     '(go declare)))

(defexpander function(whole env)
  (destructuring-bind(op a)whole
    (declare(ignore op))
    (if(symbolp a)
      whole
      (if(eq 'lambda(car a))
	(funcall(get-expander 'lambda)a env)
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
    `#'(,op ,(expand-params params env)
	    ,@(loop :for form :in body :collect (expand form env)))))

(defexpander the(whole env)
  (destructuring-bind(op type form)whole
    ; TYPE may include AND or OR form, but it is not MACROs.
    `(,op ,type ,(expand form env))))

(copy-expander 'return-from 'the)

(defexpander unwind-protect(whole env)
  (destructuring-bind(op form . cleans)whole
    `(,op ,(expand form env)
	  ,@(loop :for clean :in cleans :collect (expand clean env)))))

(defexpander throw(whole env)
  (destructuring-bind(op tag result)whole
    `(,op ,(expand tag env)
	  ,(expand result env))))

(defexpander setq(whole env)
  (destructuring-bind(op . forms)whole
    `(,op ,@(loop :for (place form) :on forms :by #'cddr
		  :collect place :collect (expand form env)))))

(copy-expander 'setf 'setq)

(defexpander if(whole env)
  (destructuring-bind(op pred then . else)whole
    `(,op ,(expand pred env)
	  ,(expand then env)
	  ,@(when else
	      `(,(expand (car else)env))))))

(defexpander locally(whole env)
  (destructuring-bind(op . body)whole
    `(,op ,@(loop :for form :in body :collect (expand form env)))))

(progn . #.(mapcar (lambda(dest)
		     `(copy-expander ',dest 'locally))
		   '(multiple-value-call multiple-value-prog1 tagbody progn progv load-time-value)))

(defexpander eval-when(whole env)
  (destructuring-bind(op cond . body)whole
    `(,op ,cond ,@(loop :for form :in body :collect(expand form env)))))

(progn . #.(mapcar (lambda(dest)
		     `(copy-expander ',dest 'eval-when))
		   '(catch block)))
