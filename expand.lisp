(in-package :cl-user)
#+sbcl(require :sb-cltl2)
(defpackage :expander(:use :cl)
  (:export ; for light users.
    ;; Main api
    #:expand
    )
  (:shadow #:macroexpand)
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
    #:*default-expander*
    ;; Conditions
    #:expander-error #:missing-expandtable #:expander-conflict
    ;; Restarts
    #:use-prev #:use-next
    ;; Helpers
    #:find-expandtable #:make-expandtable #:expand-sub-form #:expand* #:call
    ;; Walker
    #:walk-sublis
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

(defmacro trace-expanders(name)
  (let((table(find-expandtable name))
       names)
    (maphash (lambda(symbol function)
	       (declare(ignore symbol))
	       (pushnew function names :test #'eq))
	     table)
    `(trace ,@names)))

;;;; *EXPANDTABLE*, current expandtable.
(defparameter *expandtable*(find-expandtable 'standard))

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
	     `((|lambda-expander| (car form) environment)
	       ,@(expand* (cdr form)environment)))
	    (list form) ; it may just data.
	    (t (%expand form environment))))))

(prototype expand-symbol-macro((AND SYMBOL (NOT (OR KEYWORD BOOLEAN)))
			       &optional T)T)
(defun expand-symbol-macro(form &optional environment)
  (call-with-macroexpand-check form environment #'%expand))

;; MACROEXPAND
(defun macroexpand(form env)
  (multiple-value-bind(new expanded?)(macroexpand-1 form env)
    (if expanded?
      (if (eq form new) ; &whole works.
	new
	(macroexpand new env))
      new)))

;;; %EXPAND
(prototype %expand(cons &optional T)T)
(defun %expand(form &optional environment)
  (funcall (get-expander(car form))
	   form environment))

;; CALL-WITH-MACROEXPAND-CHECK
(defun call-with-macroexpand-check(form env cont)
  (let((result (macroexpand form env)))
    (if(atom result) ; may be expanded into atom directly.
      result ; else RESULT  may include macro form in its sub-forms.
      (if(typep (car result) '(cons (eql lambda)*))
	`((|lambda-expander| (car result) env)
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
	    (expand(macroexpand form env)env)
	    (expand-sub-form form env)))))))

(defvar *default-expander* '|default-expander|)

(defun get-expander(key &optional(*default-expander* *default-expander*))
  (gethash key *expandtable* *default-expander*))

(defun expand-sub-form(form env)
  `(,(car form)
     ,@(expand* (cdr form)env)))

;;;; Walker
(defun walk-sublis(binds form)
  (let((*default-expander* '|expanded-walk-default|))
    (expand `(symbol-macrolet ,binds ,form))))

(defun |expanded-walk-default|(form env)
  (call-with-macroexpand-check form env #'expand-sub-form))

;;;; OPTIMIZE table
(eval-when(:compile-toplevel :load-toplevel :execute)
  (defun cons-type-specifier(list)
    (typecase list
      (null '*)
      (atom (if (eq '* list)
	      list
	      `(eql ,list)))
      (cons `(cons ,(cons-type-specifier (car list))
		   ,(cons-type-specifier (cdr list)))))))

(defun |funcall-expander|(form env)
  (destructuring-bind(op function . args)form
    (setf function (expand function env))
    (typecase function
      ((cons (eql function)(cons symbol null))
       `(,(cadr function),@(expand* args env)))
      (#.(cons-type-specifier '#'(lambda()))
       (destructuring-bind(function(lambda lambda-list . body))function
	 (declare(ignore function lambda))
	 (if(intersectionp lambda-list lambda-list-keywords :test #'eq)
	   `(,op #'(lambda ,lambda-list ,@body) ,@(expand* args env))
	   (let((binds(loop :for var :in lambda-list
			    :for arg :in args
			    :unless (eq var arg)
			    :collect `(,var ,(expand arg env)))))
	     (if binds
	       `(let,binds,@body)
	       (if(cdr body)
		 `(locally ,@body)
		 (car body)))))))
      (#.(cons-type-specifier '(constantly *))
       (cadr function))
      (#.(cons-type-specifier '(let()))
       (multiple-value-bind(binds decls prebody main)(parse-bubble-let function)
	 (expand `(let ,binds ,@decls ,@prebody(,op ,main ,@args)))))
      (t `(,op ,function ,@(expand* args env))))))

(defun intersectionp(list1 list2 &key (key #'identity)(test #'eql)test-not)
  (when test-not (setq test (complement test-not)))
  (loop :for elt :in list2
	:thereis (member elt list1 :test test :key key)))

(defun parse-bubble-let(let)
  (destructuring-bind(op binds . body)let
    (declare(ignore op))
    (multiple-value-bind(body decls)(alexandria:parse-body body)
      (multiple-value-bind(symbol-macrolet-cadr new-binds)(parse-bubble-binds binds)
	(multiple-value-call #'values
	  new-binds
	  (loop :for declare-form :in decls
		:collect (cons 'declare (loop :for option :in (cdr declare-form)
					      :with alist = (mapcar (lambda(x)
								      (apply #'cons x))
								    symbol-macrolet-cadr)
					      :collect (case (car option)
							 ((ignore ignorable)
							  (sublis alist (cdr option)))
							 ((type)
							  (list* (car option)
								 (cadr option)
								 (sublis alist (cddr option))))
							 (t option)))))
	  (loop :for body :on body
		:for form = (walk-sublis symbol-macrolet-cadr (car body))
		:if(null(cdr body))
		:return (values prebody form)
		:else :collect form :into prebody))))))

(defun parse-bubble-binds(binds)
  (labels((rec(binds &optional sm-cadr new-binds)
	    (if (endp binds)
	      (values (nreverse sm-cadr)(nreverse new-binds))
	      (body(car binds)(cdr binds)sm-cadr new-binds)))
	  (body(bind rest sm-cadr new-binds)
	    (if(symbolp bind)
	      (let((gensym(gensym(symbol-name bind))))
		(rec rest (cons `(,bind ,gensym)sm-cadr)(cons gensym new-binds)))
	      (let((gensym(gensym(symbol-name(car bind)))))
		(rec rest (cons `(,(car bind),gensym)sm-cadr)
		     (cons `(,gensym ,(cadr bind))new-binds))))))
    (rec binds)))

(defun |append-expander|(form env)
  (destructuring-bind(op . args)form
    (let((expanded(remove-if (lambda(x)
			       (and (constantp x env)
				    (null (introspect-environment:constant-form-value x env))))
			     (flatten-nested-op 'append(expand* args env)))))
      (cond
	((null expanded)nil)
	((null(cdr expanded))
	 (car expanded))
	(t (multiple-value-bind(binds decls prebody args)(sieve-let expanded)
	     (if(null binds)
	       `(,op ,@args)
	       (expand `(let ,binds ,@decls ,@prebody (,op ,@args))))))))))

(defun sieve-let(args)
  (labels((rec(list &optional binds decls prebody args)
	    (if(endp list)
	      (values (nreverse binds) decls (nreverse prebody) (nreverse args))
	      (body (car list)(cdr list)binds decls prebody args)))
	  (body(arg rest binds-acc decls-acc prebody-acc args)
	    (if(not(typep arg '#.(cons-type-specifier '(let()))))
	      (let((var(gensym)))
		(rec rest
		     (nconc `((,var ,arg)) binds-acc)
		     decls-acc
		     prebody-acc
		     (cons var args)))
	      (multiple-value-bind(binds decls prebody main)(parse-bubble-let arg)
		(rec rest
		     (nreconc binds binds-acc)
		     (nconc decls decls-acc)
		     (nreconc prebody prebody-acc)
		     (cons main args))))))
    (if(find-if (lambda(x)(typep x '#.(cons-type-specifier '(let()))))
		args)
      (rec args)
      (values nil nil nil args))))

(defun flatten-nested-op(op expanded &key (args #'cdr))
  (labels((rec(append-args &optional acc)
	    (if(endp append-args)
	      acc
	      (body (car append-args)(cdr append-args)acc)))
	  (body(arg rest acc)
	    (if(and (listp arg)
		    (progn (when (eq 'the (car arg))
			     (setf arg (third arg)))
			   (eq op (car arg))))
	      (rec rest (append (nreverse(rec (funcall args arg)))
				acc))
	      (rec rest (cons arg acc)))))
    (rec (reverse expanded))))

(defun |mapcar-expander|(form env)
  (destructuring-bind(op fun . args)form
    (setf fun (expand fun env))
    (setf args (expand* args env))
    (cond
      ((loop :for form :in args
	     :thereis (and (constantp form env)
			   (null(introspect-environment:constant-form-value form env))))
       (let((args(remove-if (lambda(x)(constantp x env))
			    args)))
	 (if args
	   `(progn ,@args nil)
	   nil)))
      (t `(,op ,fun ,@args)))))

(defun |list-expander|(form env)
  (if(cdr form)
    (expand-sub-form form env)
    nil))

(defun |vector-expander|(form env)
  (if(cdr form)
    (expand-sub-form form env)
    #()))

(defun |concatenate-expander|(form env)
  (destructuring-bind(op type . rest)form
    (let*((expanded (remove-if (lambda(x)
				 (and (constantp x env)
				      (alexandria:emptyp (introspect-environment:constant-form-value x env))))
			       (flatten-nested-op 'concatenate (expand* rest env) :args #'cddr))))
      (cond
	((null expanded)
	 (if(constantp type env)
	   (coerce nil (introspect-environment:constant-form-value type))
	   `(coerce nil ,type)))
	((null(cdr expanded))
	 `(coerce ,(car expanded) ,type))
	(t (multiple-value-bind(binds decls prebody args)(sieve-let expanded)
	     (if(null binds)
	       `(,op ,type ,@expanded)
	       (expand `(let,binds,@decls,@prebody (,op ,type ,@args))))))))))

(handler-bind((expander-conflict #'use-next))
  (defexpandtable optimize
    (:use standard)
    (:add |funcall-expander| funcall)
    (:add |append-expander| append)
    (:add |mapcar-expander| mapcar maplist mapcan mapcon)
    (:add |list-expander| list)
    (:add |vector-expander| vector)
    (:add |concatenate-expander| concatenate)
    ))

