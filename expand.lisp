(in-package :cl-user)
#+sbcl(require :sb-cltl2)
(defpackage :expander(:use :cl)
  (:shadow #:macroexpand)
  (:import-from #.(or #+sbcl :sb-cltl2
		      #+ccl :ccl
		      (error "~A is not supported." (lisp-implementation-type)))
		#:augment-environment
		#:parse-macro
		#:enclose)

  (:export ; for light users.
    ;; Main api
    #:expand
    )
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
    #:find-expandtable #:make-expandtable #:expand-sub-form #:expand*
    ;; Walker
    #:walk-sublis
    )
  (:export ; helpers to extension.
    #:parse-bubble-let #:expand-params #:remove-the #:pure-fun-form-p
    )
  (:export ; debug use
    #:call #:expand-with #:with-trace-out #:trace-expanders
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
  (or (and (hash-table-p name)
	   name)
      (gethash name *expandtables*)
      (when errorp
	(restart-case(error 'missing-expandtable :name name)
	  (use-value(value) :report "Use altanative expandtable."
			    :interactive (lambda()
					   (format *query-io* "Altenative > ")
					   (force-output *query-io*)
					   (list(read)))
			    (find-expandtable value))))))

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

(defun expand-with(name form)
  (let((*expandtable* (find-expandtable name)))
    (expand form)))

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
    (multiple-value-bind(body decls)(alexandria:parse-body body)
      `#'(,op ,(expand-params params env)
	      ,@decls
	      ,@(expand* body (Augment-environment
				env
				:variable (lambda-fiddle:extract-all-lambda-vars params)
				:declare (alexandria:mappend #'cdr decls)))))))

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
  (:add |setq-expander| setq)
  (:add |if-expander| if)
  (:add |locally-expander| locally multiple-value-call multiple-value-prog1 tagbody progn progv load-time-value catch)
  (:add |eval-when-expander| eval-when block)
  )

(defmacro trace-expanders(name)
  (let((table(find-expandtable name))
       names)
    (maphash (lambda(symbol function)
	       (declare(ignore symbol))
	       (pushnew function names :test #'eq))
	     table)
    `(trace ,@names)))

(defmacro with-trace-out((expandtable filename)form)
  `(unwind-protect (with-open-file(*trace-output* ,filename :direction :output
						  :if-does-not-exist :create
						  :if-exists :supersede)
		     (trace-expanders ,expandtable)
		     (expand-with ',expandtable ',form))
     (untrace)))

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
	     `(,(second(|lambda-expander| (car form) environment))
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
	`(,(second(|lambda-expander| (car result) env))
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

(defun remove-the(form)
  (if(and (listp form)
	  (eq 'the (car form)))
    (remove-the (third form))
    form))

(defun |funcall-expander|(form env)
  (destructuring-bind(op function . args)form
    (setf function (remove-the (expand function env)))
    (typecase function
      ((cons (eql function)(cons symbol null))
       `(,(cadr function),@(expand* args env)))
      (#.(cons-type-specifier '#'(lambda()))
       (destructuring-bind(function(lambda lambda-list . body))function
	 (declare(ignore function lambda))
	 (if(intersectionp lambda-list lambda-list-keywords :test #'eq)
	   `(,op #'(lambda ,lambda-list ,@body) ,@(expand* args env))
	   (let((binds(mapcar #'list lambda-list args)))
	     (if binds
	       `(let,binds,@body)
	       (if(cdr body)
		 `(locally ,@body)
		 (car body)))))))
      (#.(cons-type-specifier '(constantly *))
       (let((arg-forms(remove-if (lambda(x)(constantp x env))
				 (expand* args env))))
	 (if arg-forms
	   `(progn ,@arg-forms ,(cadr function))
	   (cadr function))))
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
							  (cons (car option)(sublis alist (cdr option))))
							 ((type)
							  (list* (car option)
								 (cadr option)
								 (sublis alist (cddr option))))
							 (t option)))))
	  (loop :for body :on body
		:with *expandtable* = (handler-bind((expander-conflict #'use-next))
					(make-expandtable `((:use ,*expandtable*)
							    (:add |bubble-up-declare-expander| declare))))
		:for form = (walk-sublis symbol-macrolet-cadr (car body))
		:if(null(cdr body))
		:return (values prebody form)
		:else :collect form :into prebody))))))

(defun |bubble-up-declare-expander|(form env)
  (cons (car form)
	(loop :for option :in (cdr form)
	      :collect (case(car option)
			 ((ignore ignorable)
			  (cons (car option)(expand* (cdr option)env)))
			 ((type)
			  (list* (car option)
				 (cadr option)
				 (expand* (cddr option) env)))
			 (t option)))))

(defun parse-bubble-binds(binds)
  (labels((rec(binds &optional sm-cadr new-binds)
	    (if (endp binds)
	      (values (nreverse sm-cadr)(nreverse new-binds))
	      (body(car binds)(cdr binds)sm-cadr new-binds)))
	  (body(bind rest sm-cadr new-binds)
	    (if(symbolp bind)
	      (let((gensym(gensym(remove-if #'digit-char-p(symbol-name bind)))))
		(rec rest (cons `(,bind ,gensym)sm-cadr)(cons gensym new-binds)))
	      (let((gensym(gensym(remove-if #'digit-char-p(symbol-name(car bind))))))
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
	(t (multiple-value-bind(let-form args)(bubble-up expanded env)
	     (if let-form
	       (expand `(,@let-form (,op ,@args)) env)
	       `(,op ,@args))))))))

(defun bubble-up(args &optional env)
  (labels((rec(args &optional main)
	    (if(endp args)
	      (values nil (nreverse main))
	      (body (car args)(cdr args)main)))
	  (body(form rest main)
	    (if(constantp form env)
	      (rec rest (cons form main))
	      (if (not(typep form '#.(cons-type-specifier '(let()))))
		(values nil (nreconc main (cons form rest)))
		(multiple-value-bind(binds decls prebody let-main)(parse-bubble-let form)
		  (values `(let,binds,@decls,@prebody)
			  (nreconc main (cons let-main rest))))))))
    (rec args)))

(defun flatten-nested-op(op args-list &key (args #'cdr))
  (labels((rec(list &optional acc)
	    (if(endp list)
	      acc
	      (body (remove-the(car list))(cdr list)acc)))
	  (body(form rest acc)
	    (if (and (listp form)
		     (eq op (car form)))
	      (rec rest (rec (funcall args form)acc))
	      (rec rest (cons form acc)))))
    (nreverse (rec args-list))))

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
	   (if(pure-fun-form-p fun env)
	     `(progn ,@args nil)
	     `(progn ,fun ,@args nil))
	   (if(pure-fun-form-p fun env)
	     nil
	     `(progn ,fun nil)))))
      (t `(,op ,fun ,@args)))))

(defun pure-fun-form-p(form env)
  (or (constantp form env)
      (and (symbolp form)
	   (let((type(introspect-environment:variable-information form env)))
	     (if(member type '(nil :symbol-macro) :test #'eq)
	       (return-from pure-fun-form-p nil)
	       T)))
      (typep form '(cons (eql function)(cons symbol null)))
      (typep form '(cons (eql function)
			 (cons (cons (eql lambda)*)
			       *)))))

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
    (setf type (expand type env))
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
	(t (if(not(constantp type env)) ; to keep eval order.
	     `(,op ,type ,@expanded)
	     (multiple-value-bind(let-form args)(bubble-up expanded env)
	       (if let-form
		 (expand `(,@let-form (,op ,type ,@args))env)
		 `(,op ,type ,@expanded)))))))))

(defun |*-expander|(form env)
  (let((expanded(remove-if (lambda(x)
			     (and (constantp x env)
				  (eql 1 (introspect-environment:constant-form-value x env))))
			   (flatten-nested-op '*
					      (let((*expandtable*(find-expandtable 'standard)))
						(expand* (cdr form) env))))))
    (multiple-value-bind(zerop args)(sieve-zero expanded env)
      (if zerop
	(if args
	  `(progn ,@args 0)
	  0)
	(cond
	  ((null expanded)(*))
	  ((null(cdr expanded))
	   (car expanded))
	  (t (multiple-value-bind(let-form args)(bubble-up expanded env)
	       (if let-form
		 (expand `(,@let-form (,(car form),@args))env)
		 `(,(car form),@expanded)))))))))

(defun sieve-zero(expanded env)
  (loop :for form :in expanded
	:with zerop = nil
	:if (and (constantp form env)
		 (eql 0 (introspect-environment:constant-form-value form env)))
	:do (setf zerop t)
	:else :unless (constantp form env)
	:collect form :into args
	:finally (return (values zerop args))))

(defun |+-expander|(form env)
  (let((expanded(remove-if (lambda(x)
			     (and (constantp x env)
				  (eql 0 (introspect-environment:constant-form-value x env))))
			   (flatten-nested-op '+ (expand* (cdr form) env)))))
    (cond
      ((null expanded)(+))
      ((null(cdr expanded))
       (car expanded))
      (t (multiple-value-bind(let-form args)(bubble-up expanded env)
	   (if let-form
	     (expand `(,@let-form (,(car form),@args)) env)
	     `(,(car form),@expanded)))))))

; Remove-null-bind, e.g. (let()a)
; Expand-useless-bind, e.g. (let((a b))a) => b
(defun |optimized-let-expander|(form env)
  (destructuring-bind(op binds . body)form
    (multiple-value-bind(body decls)(alexandria:parse-body body)
      (setf body (expander:expand* body env))
      (cond
	;; Remove-null-bind
	((and (null binds)
	      (null decls)
	      (null(cdr body)))
	 (car body))
	;; Expand-useless-bind
	((and (null (cdr body)) ; one body.
	      (null (cdr binds)) ; one bind.
	      (eq (car body) (alexandria:ensure-car (car binds))))
	 (if(atom(car binds))
	   nil
	   (expander:expand (cadar binds) env)))
	(t `(,op,(loop :for elt :in binds
		       :if(symbolp elt):collect elt
		       :else :collect
		       `(,(car elt),(expand(cadr elt)env)))
	      ,@decls
	      ,@(expand* body (Augment-environment
				env
				:variable (mapcar #'alexandria:ensure-car binds)
				:declare (alexandria:mappend #'cdr decls)))))))))

(defun |optimized-let*-expander|(form env)
  (destructuring-bind(op binds . body)form
    (multiple-value-bind(body decls)(alexandria:parse-body body)
      (setf body (expander:expand* body env))
      (cond
	;; Remove-null-bind
	((and (null binds)
	      (null decls)
	      (null(cdr body)))
	 (car body))
	;; Expand-useless-bind
	((and (null (cdr body)) ; one body.
	      (null (cdr binds)) ; one bind.
	      (eq (car body) (alexandria:ensure-car (car binds))))
	 (if(atom(car binds))
	   nil
	   (expander:expand (cadar binds) env)))
	(t `(,op,(loop :for elt :in binds
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
				env))))))))

(defun |optimized-if-expander|(form env)
  (destructuring-bind(pred then . else)(expand* (cdr form)env)
    (if(constantp pred env)
      (if(introspect-environment:constant-form-value pred env)
	then
	(car else))
      (list* (car form) pred then else))))

(defun |optimized-locally-expander|(form env)
  (destructuring-bind(op . body)form
    (multiple-value-bind(body decls)(alexandria:parse-body(expander:expand* body env))
      (setf decls (remove '(declare) decls :test #'equal))
      (if decls
	`(,op ,@decls ,@body)
	(if(cdr body)
	  `(progn ,@body)
	  (car body))))))

(defun |optimized-block-expander|(form env)
  (destructuring-bind(op tag . body)form
    (setf body (expander:expand* body env))
    (cond
      ((null body)nil)
      ((and (null (cdr body))
	    (constantp (car body)env))
       (car body))
      ((typep (car body) `(cons (eql return-from)(cons (eql ,tag) *)))
       (third(car body)))
      ;; If block does not have return-from, no need to achive block.
      ((not (trestrul:find-node-if (lambda(x)
				     (typep x `(cons (eql return-from)
						     (cons (eql ,tag)
							   (cons * null)))))
				   body))
       (if(cdr body)
	 `(progn ,@body)
	 (car body)))
      (t (let*((pos(position-if (lambda(x)
				  (typep x '(cons (eql return-from) *)))
				body))
	       ;; If body has return-from form, we can ignore rest forms.
	       (body (if pos
		       (subseq body 0 (1+ pos))
		       body)))
	   (multiple-value-bind(let-form forms)(bubble-up body env)
	     (if let-form
	       (expand `(,@let-form (,op ,tag ,@forms))env)
	       `(,op ,tag ,@body))))))))

(defun |optimized-return-from-expander|(form env)
  (destructuring-bind(op tag body)form
    (setf body (expander:expand body env))
    ;; To reduce nested return-from, e.g. (return-from a (return-from b ...))
    ;; In the example above, `A` can be ignored.
    (if(and (listp body)
	    (eq 'return-from (car body)))
      body
      (multiple-value-bind(let-form main)(bubble-up (list body) env)
	(if let-form
	  (expand `(,@let-form (,op ,tag ,@main)) env)
	  `(,op ,tag ,body))))))

(defun |format-expander|(form env)
  (destructuring-bind(op target string . args)form
    (if(constantp string env)
      `(,op ,(expand target env)
	    ,(expand `(formatter ,(introspect-environment:constant-form-value string env))
		     env)
	    ,@(expand* args env))
      (expand-sub-form form env))))

(defun |optimized-the-expander|(form env)
  (destructuring-bind(op type body)form
    (setf body (expand body env))
    (if(and (listp body)
	    (eq 'the (car body))
	    (equal type (cadr body)))
      body
      `(,op ,type ,body))))

(handler-bind((expander-conflict #'use-next))
  (defexpandtable optimize
    (:use standard)
    (:add |funcall-expander| funcall)
    (:add |append-expander| append)
    (:add |mapcar-expander| mapcar maplist mapcan mapcon)
    (:add |list-expander| list)
    (:add |vector-expander| vector)
    (:add |concatenate-expander| concatenate)
    (:add |*-expander| *)
    (:add |+-expander| +)
    (:add |optimized-let-expander| let)
    (:add |optimized-let*-expander| let*)
    (:add |optimized-if-expander| if)
    (:add |optimized-locally-expander| locally)
    (:add |optimized-block-expander| block)
    (:add |optimized-return-from-expander| return-from)
    (:add |format-expander| format)
    (:add |optimized-the-expander| the)
    ))

