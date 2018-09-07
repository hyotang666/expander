(in-package :cl-user)
(defpackage :expander(:use :cl)
  (:export ; for light users.
    ;; Main api
    #:expand
    )
  (:import-from #.(or #+sbcl :sb-cltl2
		      #+ccl :ccl
		      (error "~A is not supported." (lisp-implementation-type)))
		#:augment-environment
		#:parse-macro
		#:enclose)
  (:export ; for hackers.
    ;; Main DSL
    #:defexpandtable
    ;; Special variable
    #:*expandtable* ; current expandtable.
    ;; Conditions
    #:expander-error #:missing-expandtable #:expander-conflict
    ;; Restarts
    #:use-prev #:use-next
    ;; Helpers
    #:find-expandtable #:make-expandtable #:expand-sub-form #:expand* #:call
    )
  )
(in-package :expander)

(eval-when(:compile-toplevel :load-toplevel :execute)
  (defmacro prototype(name param-types return-types)
    `(DECLAIM(FTYPE(FUNCTION ,param-types ,return-types),name)))
  )

#| DSL |#
(defvar *expandtables* (make-hash-table :test #'eq))

;;;; DEFEXPANDTABLE
(defmacro defexpandtable(name &rest clause)
  ;; Trivial syntax checks
  (check-type name symbol)
  (assert (every #'listp clause))
  (dolist(c clause)
    (assert (listp c))
    (ecase (car c)
      (:use (mapc #'find-expandtable (cdr c)))
      (:add (assert (every #'symbolp (cdr c))))))
  ;; body
  `(SETF (GETHASH ',name *EXPANDTABLES*)
	 (MAKE-EXPANDTABLE ',clause)))

;;;; CONDITIONS
(define-condition expander-error(error)())
(define-condition missing-expandtable(expander-error cell-error)
  ()
  (:report (lambda(condition stream)
	     (format stream "Expandtable named ~S is not found."
		     (cell-error-name condition)))))
(define-condition expander-conflict(expander-error cell-error)
  ()
  (:report (lambda(condition stream)
	     (format stream "Expander conflicts. ~S"
		     (cell-error-name condition)))))

;;;; RESTARTS
(defun use-prev(condition)
  (let((restart(find-restart 'use-prev condition)))
    (when restart
      (invoke-restart restart))))

(defun use-next(condition)
  (let((restart(find-restart 'use-next condition)))
    (when restart
      (invoke-restart restart))))

;;;; FIND-EXPANDTABLE
(defun find-expandtable(name &optional (errorp t))
  (or (gethash name *expandtables*)
      (when errorp
	(error 'missing-expandtable :name name))))

;;;; MAKE-EXPANDTABLE
(defun make-expandtable(clauses)
  (let((ht(make-hash-table :test #'eq)))
    (flet((ADD-EXPANDER(key value)
	    (if(null(gethash key ht))
	      (setf (gethash key ht) value)
	      (restart-case(error 'expander-conflict :name key)
		(use-prev() :report "Use previous expander, discard new one.")
		(use-next() :report "Use new expander, discard old one."
			    (setf (gethash key ht)value))))))
      (dolist(clause clauses ht)
	(ecase (car clause)
	  (:use (dolist(elt (cdr clause))
		  (maphash #'ADD-EXPANDER (find-expandtable elt))))
	  (:add (loop :with value = (cadr clause)
		      :for key :in (cddr clause)
		      :do (ADD-EXPANDER key value))))))))

;;;; Standard expandtable.
(defun |quote-expander|(whole env)
  (declare(ignore env))
  whole)

(defun |function-expander|(whole env)
  (destructuring-bind(op a)whole
    (declare(ignore op))
    (if(symbolp a)
      whole
      (if(eq 'lambda(car a))
	(funcall(get-expander 'lambda)a env)
	whole))))

(defun |macrolet-expander|(whole env)
  (destructuring-bind(op binds . body)whole
    (declare(ignore op))
    (multiple-value-bind(body decls)(alexandria:parse-body body)
      (flet((new-env(decls)
	      (Augment-environment
		env
		:macro (mapcar (lambda(bind)
				 (destructuring-bind(name lambda-list . body)bind
				   `(,name,(Enclose
					     (Parse-macro name
							  lambda-list
							  body
							  env)
					     env))))
			       binds)
		:declare (alexandria:mappend #'cdr decls))))
	(if (cdr body)
	  `(progn ,(expand* body (new-env decls)))
	  (expand (car body) (new-env decls)))))))

(defun |symbol-macrolet-expander|(whole env)
  (destructuring-bind(op binds . body)whole
    (declare(ignore op))
    (multiple-value-bind(body decls)(alexandria:parse-body body)
      (flet((new-env(decls)
	      (Augment-environment
		env
		:symbol-macro binds
		:declare (alexandria:mappend #'cdr decls))))
	(if(cdr body)
	  `(progn ,(expand* body (new-env decls)))
	  (expand (car body) (new-env decls)))))))

(defun |let-expander|(whole env)
  (destructuring-bind(op binds . body)whole
    (multiple-value-bind(body decls)(alexandria:parse-body body)
      `(,op,(loop :for elt :in binds
		  :if(symbolp elt):collect elt
		  :else :collect
		  `(,(car elt),(expand(cadr elt)env)))
	 ,@decls
	 ,@(expand* body (Augment-environment
			   env
			   :variable (mapcar #'alexandria:ensure-car binds)
			   :declare (alexandria:mappend #'cdr decls)))))))

(defun |let*-expander|(whole env)
  (destructuring-bind(op binds . body)whole
    (multiple-value-bind(body decls)(alexandria:parse-body body)
      `(,op ,(loop :for elt :in binds
		   :if (symbolp elt)
		   :collect (progn (setf env (Augment-environment
					       env
					       :variable (list elt)))
				   elt)
		   :else :collect `(,(car elt)
				     ,(expand (cadr elt)
					      (setf env (Augment-environment
							  env
							  :variable (list (car elt)))))))
	    ,@decls
	    ,@(expand* body (if decls
			      (Augment-environment
				env
				:declare (alexandria:mappend #'cdr decls))
			      env))))))

(defun expand*(sub-forms env)
  (mapcar (lambda(sub-form)
	    (expand sub-form env))
	  sub-forms))

(defun |flet-expander|(whole env)
  (destructuring-bind(op binds . body)whole
    (multiple-value-bind(body decls)(alexandria:parse-body body)
      `(,op,(loop :for (name params . body):in binds :collect
		  `(,name ,(expand-params params env)
			  ,@(loop :for elt :in body :collect
				  (expand elt env))))
	 ,@decls
	 ,@(expand* body (Augment-environment
			   env
			   :function (mapcar #'car binds)))))))

(defun |labels-expander|(whole env)
  (destructuring-bind(op binds . body)whole
    (multiple-value-bind(body decls)body
      `(,op ,(loop :for (name params . body) :in binds
		   :do (setf env (Augment-environment
				   env
				   :function (list name)))
		   :collect `(,name ,(expand-params params env)
				    ,@(expand* body env)))
	    ,@decls
	    ,@(expand* body (Augment-environment
			      env
			      :declare (alexandria:mappend #'cdr decls)))))))

(defun expand-params(params env)
  (loop :for param :in params
	:if (symbolp param) :collect param
	:else :collect
	`(,(car param),(expand(cadr param)env)
	   ,@(when(caddr param)
	       `(,(caddr param))))))

(defun |lambda-expander|(whole env)
  (destructuring-bind(op params . body)whole
    `#'(,op ,(expand-params params env)
	    ,@(expand* body env))))

(defun |the-expander|(whole env)
  (destructuring-bind(op type form)whole
    ; TYPE may include AND or OR form, but it is not MACROs.
    `(,op ,type ,(expand form env))))

(defun |unwind-protect-expander|(whole env)
  (destructuring-bind(op form . cleans)whole
    `(,op ,(expand form env)
	  ,@(expand* cleans env))))

(defun |throw-expander|(whole env)
  (destructuring-bind(op tag result)whole
    `(,op ,(expand tag env)
	  ,(expand result env))))

(defun |setq-expander|(whole env)
  (destructuring-bind(op . forms)whole
    `(,op ,@(loop :for (place form) :on forms :by #'cddr
		  :collect place :collect (expand form env)))))

(defun |if-expander|(whole env)
  (destructuring-bind(op pred then . else)whole
    `(,op ,(expand pred env)
	  ,(expand then env)
	  ,@(when else
	      `(,(expand (car else)env))))))

(defun |locally-expander|(whole env)
  (destructuring-bind(op . body)whole
    `(,op ,@(expand* body env))))

(defun |eval-when-expander|(whole env)
  (destructuring-bind(op cond . body)whole
    `(,op ,cond ,@(expand* body env))))

;;;; Standard expandtable
(defexpandtable standard
  (:add |quote-expander| quote go declare)
  (:add |function-expander| function)
  (:add |macrolet-expander| macrolet)
  (:add |symbol-macrolet-expander| symbol-macrolet)
  (:add |let-expander| let)
  (:add |let*-expander| let*)
  (:add |flet-expander| flet)
  (:add |labels-expander| labels)
  (:add |lambda-expander| lambda)
  (:add |the-expander| the return-from)
  (:add |unwind-protect-expander| unwind-protect)
  (:add |throw-expander| throw)
  (:add |setq-expander| setq setf)
  (:add |if-expander| if)
  (:add |locally-expander| locally multiple-value-call multiple-value-prog1 tagbody progn progv load-time-value)
  (:add |eval-when-expander| eval-when catch block)
  )

;;;; *EXPANDTABLE*, current expandtable.
(defvar *expandtable*(find-expandtable 'standard))

;;; To debug, or to test.
(defun call(symbol form &optional(name 'standard))
  (let((*expandtable*(find-expandtable name)))
    (funcall(get-expander symbol)form nil)))

;;;; EXPAND
(prototype expand(T &optional T)T)
(defun expand(form &optional environment)
  (etypecase form
    ((AND SYMBOL(NOT(OR BOOLEAN KEYWORD)))
     ;; it may symbol-macro, so...
     (expand-symbol-macro form environment))
    (ATOM form)
    (LIST (typecase (car form)
	    ((cons (eql lambda) *) ; ((lambda()...)...)
	     `((LAMBDA,(cadar form),@(expand* (cddar form)environment))
	       ,@(expand* (cdr form)environment)))
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

;; CALL-WITH-MACROEXPAND-CHECK
(defun call-with-macroexpand-check(form env cont)
  (let((result (macroexpand% form env)))
    (if(atom result) ; may be expanded into atom directly.
      result ; else RESULT  may include macro form in its sub-forms.
      (if(typep (car result) '(cons (eql lambda)*))
	`((LAMBDA,(cadar result),@(expand* (cddar result)env))
	  ,@(expand* (cdr result)env))
	(funcall (get-expander (car result) cont)
		 result env)))))

(defun |default-expander|(form env)
  (let((cmf(compiler-macro-function(car form)env)))
    (if(null cmf)
      (call-with-macroexpand-check form env #'expand-sub-form)
      (let((new(funcall *macroexpand-hook* cmf form env)))
	(if(not(eq new form))
	  (expand new env)
	  ;; else compiler macro may be defined on macro.
	  (if(macro-function(car form))
	    (expand(macroexpand% form env)env)
	    (expand-sub-form form env)))))))

(defun get-expander(key &optional(default '|default-expander|))
  (gethash key *expandtable* default))

(defun expand-sub-form(form env)
  `(,(car form)
     ,@(expand* (cdr form)env)))
