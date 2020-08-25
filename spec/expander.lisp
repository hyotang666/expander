(defpackage :expander.spec
  (:use :cl :jingoh :expander))
(in-package :expander.spec)
(setup :expander)

(requirements-about EXPAND)

;;;; Description:

#+syntax
(EXPAND form &optional environment) ; => result

;;;; Arguments and Values:

; form := 

; environment := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests.
;; Return-from's second argument is optional.
#?(expand '(return-from name))
=> (RETURN-FROM NAME)
,:test equal

(requirements-about DEFEXPANDTABLE)

;;;; Description:

#+syntax
(DEFEXPANDTABLE name &rest clause*) ; => result

;;;; Arguments and Values:

; name := symbol, otherwise error.
#?(defexpandtable "NOT symbol" ) :signals error

; clause := [ use-clause | add-clause ]
; use-clause := (:use expandtable-name*)
; expandtable-name := symbol which name already defined expandtable, otherwise error.
#?(defexpandtable dummy (:use not-exist))
:signals missing-expandtable
,:lazy t
; add-clause := (:add expander-function-name expander-name*)
; expander-function-name := symbol which names (function(form environment)form).
; expander-name := symbol, otherwise error.
#?(defexpandtable dummy (:add #'invalid))
:signals error
,:lazy t
#?(defexpandtable dummy (:add dummy "not symbol"))
:signals error
,:lazy t

; result := hash-table

;;;; Affected By:
; Existing `EXPANDTABLE`s

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:
; When expander name is conflicted, signals an error.
#?(defexpandtable dummy (:add dummy :a)(:add dummy :a))
:signals expander-conflict

(requirements-about *EXPANDTABLE*)

;;;; Description:
; The value of *expandtable* is called the current expandtable.
; It controls the macro expansion behavior of the `EXPAND`.
; See `DEFEXPANDTABLE`, `FIND-EXPANDTABLE`, `MAKE-EXPANDTABLE`.

;;;; Value type is HASH-TABLE
#? *EXPANDTABLE* :be-the hash-table

; Initial value is #<HASH-TABLE :TEST EQ :COUNT 28 {9A2F4001}>
; which conforms to expand Common Lisp special operators.

;;;; Affected By:
; `EXPAND`

;;;; Notes:

(requirements-about EXPANDER-ERROR)

;;;; Description:
; Root condition for EXPANDER system.
;;;; Class Precedence List: (case in SBCL)
; expander-error error serious-condition condition slot-object t

;;;; Effective Slots:

;;;; Notes:

(requirements-about MISSING-EXPANDTABLE)

;;;; Description:
; Signaled when specified expandtable is not found.
;;;; Class Precedence List: (case in SBCL)
; missing-expandtable expander-error cell-error error serious-condition condition slot-object t

;;;; Effective Slots:

; NAME [Type] T
; [READER] cell-error-name

;;;; Notes:

(requirements-about EXPANDER-CONFLICT)

;;;; Description:
; Signaled when expander names are conflicted.
;;;; Class Precedence List: (case in SBCL)
; expander-conflict expander-error cell-error error serious-condition condition slot-object t

;;;; Effective Slots:

; NAME [Type] T
; [READER] cell-error-name

;;;; Notes:

(common-requirements-about (USE-PREV USE-NEXT) :as op)

;;;; Description:
; Transfers control to the most recently established applicable restart having the same name of the function.

; If no such restart exists, return nil.

; When condition is non-nil, only those restarts are considered that are either explicitly associated with thart condition, or not associated with any condition; that is the excluded restarts are those that are associated with a non-empty set of conditions of which the given condition is not an element.
; If condition is nil, all restarts are considered.

; As restart, these are generally part of protocols where there is two ways to chose.
; In `EXPANDER` system, this is used to resolve expander name confliction.

;;;; Arguments and Values:

; condition := a condition object, or nil, otherwise error.
#?(op "Not condition") :signals error

; result := nil

;;;; Affected By:
; Presence of a restart having the same name.

;;;; Side-Effects:
; A transfer of control may occur if an appropriate restart is available.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about USE-PREV)

#+syntax
(USE-PREV condition) ; => result

#?(gethash :a (handler-bind((expander-conflict #'use-prev))
		(make-expandtable '((:add first :a)(:add second :a)))))
=> FIRST

(requirements-about USE-NEXT)

#+syntax
(USE-NEXT condition) ; => result

#?(gethash :a (handler-bind((expander-conflict #'use-next))
		(make-expandtable '((:add first :a)(:add second :a)))))
=> SECOND

(requirements-about FIND-EXPANDTABLE)

;;;; Description:
; If `NAME` is names already defined expandtable, return it.
; If there is no such expandtable, `FIND-EXPANDTABLE` returns `NIL` when `ERRORP` is specified `NIL`, otherwise signals error.
; See also `DEFEXPANDTABLE`.

#+syntax
(FIND-EXPANDTABLE name &optional errorp) ; => result

;;;; Arguments and Values:

; name := T, but expect symbol which names expandtable.

; errorp := generalized boolean. The default it T.

; result := hash-table whicn represents expandtable, otherwise nil when `ERRORP` specified `NIL`.
#?(find-expandtable 'standard) :be-the hash-table
#?(find-expandtable 'not-exist) :signals missing-expandtable
#?(find-expandtable 'not-exist nil) => NIL

;;;; Affected By:
; The set of expandtables created by the implementation.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MAKE-EXPANDTABLE)

;;;; Description:
; Constructs and returns a expandtable.

#+syntax
(MAKE-EXPANDTABLE clauses) ; => result

;;;; Arguments and Values:

; clauses := ([ use-clause | add-clause ]*), if not list, an error is signaled.
#?(make-expandtable "Not list") :signals error
; use-clause := (:use expander-function-name expander-name*)
; add-clause := (:add expandtable-name*)
; If Unknown clause key comes, an error is signaled.
#?(make-expandtable '((:unknown #:dummy))) :signals error
; expander-function-name := symbol which names (function(form environment)form).
; expander-name := symbol
; expandtable-name := symbol which names already defined expandtable.
; If specified expandtable is not found, an error is signaled.
#?(make-expandtable '((:use not-exist))) :signals missing-expandtable

; result := hash-table.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; If expander name conflicts, `EXPANDER-CONFLICT` is signaled.
#?(make-expandtable '((:add first conflict)(:add second conflict)))
:signals expander-conflict

(requirements-about EXPAND-SUB-FORMS)

;;;; Description:

#+syntax
(EXPAND-SUB-FORMS sub-forms env) ; => result

;;;; Arguments and Values:

; sub-forms := 

; env := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about funcall :test equal)

#?(call 'funcall '(funcall #'+ 1 2 3) 'optimize)
=> (+ 1 2 3)

#?(call 'funcall '(funcall '+ 1 2 3) 'optimize)
=> (funcall '+ 1 2 3)

#?(call 'funcall '(funcall (lambda(a &optional b)(list a b)) 1 2) 'optimize)
=> (funcall #'(lambda(a &optional b)(list a b)) 1 2)

#?(call 'funcall '(funcall (lambda(a b)(list a b)) 1 2) 'optimize)
=> (let((a 1)(b 2))(list a b))

#?(call 'funcall '(funcall (lambda(x)(print x)) x) 'optimize)
=> (let((x x))
     (print x))

#?(call 'funcall '(funcall (lambda(x)(print x)(+ x 3)) x) 'optimize)
=> (let((x x))
     (print x)
     (+ x 3))

#?(call 'funcall '(funcall (lambda(x y)(list x y)) 1 y) 'optimize)
=> (let((x 1)
	(y y))
     (list x y))

#?(call 'funcall '(funcall (constantly 0) 1 2 3) 'optimize)
=> 0

#?(call 'funcall '(funcall (let((a 0)(b 1))(constantly a)) 1 2 3) 'optimize)
=> (let((a 0)(b 1)) a)
,:test jingoh.tester:sexp=

(requirements-about bubble-up)

#?(expander::bubble-up '(1 2 3))
:values (NIL (1 2 3))

#?(expander::bubble-up '((let(a)(list a)) 1 2))
:multiple-value-satisfies
(lambda($result1 $result2)
  (& (jingoh.tester:sexp= $result1 '(let(a)))
     (jingoh.tester:sexp= $result2 '((list a) 1 2))))

#?(expander::bubble-up '((let((a 0))(declare(type fixnum a))(list a)) 1 2))
:multiple-value-satisfies
(lambda($result1 $result2)
  (& (jingoh.tester:sexp= $result1 '(let((a 0))(declare(type fixnum a))))
     (jingoh.tester:sexp= $result2 '((list a) 1 2))))

#?(expander::bubble-up '((let((a 0))(print a)(list a)) 1 2))
:multiple-value-satisfies
(lambda($result1 $result2)
  (& (jingoh.tester:sexp= $result1 '(let((a 0))(print a)))
     (jingoh.tester:sexp= $result2 '((list a)1 2))))

#?(expander::bubble-up '(1 (let((a 0))(list a)) 2))
:multiple-value-satisfies
(lambda($value1 $value2)
  (& (jingoh.tester:sexp= $value1 '(let((a 0))))
     (jingoh.tester:sexp= $value2 '(1 (list a) 2))))

#?(expander::bubble-up '(var (let((a 0))(list a)) 2))
:values (nil (var (let((a 0))(list a)) 2))

#?(expander::bubble-up '((let(a)(list a))(let(b)(list b)) 3))
:multiple-value-satisfies
(lambda($result1 $result2)
  (& (jingoh.tester:sexp= $result1 '(let(a)))
     (jingoh.tester:sexp= $result2 '((list a)(let(b)(list b))3))))

(requirements-about append :test equal)

#?(call 'append '(append) 'optimize) => NIL

#?(call 'appand '(append '(1 2 3)) 'optimize) => '(1 2 3)

#?(call 'append '(append nil nil nil) 'optimize) => NIL

#?(call 'append '(append nil '(1 2 3) nil) 'optimize) => '(1 2 3)

#?(call 'append '(append '(1) (append '(2))) 'optimize) => (append '(1)'(2))

#?(call 'append '(append '(1) (append '(2) '(3)) (append '(4)'(5))) 'optimize)
=> (append '(1)'(2)'(3)'(4)'(5))

#?(call 'append '(append '(1) '(2) (append '(3) (append '(4))'(5))) 'optimize)
=> (append '(1)'(2)'(3)'(4)'(5))

#?(call 'append '(append (let((a 0))
			   (append (list a) var))
			 var2)
	'optimize)
=> (let((a 0))
     (append (list a) var var2))
,:test jingoh.tester:sexp=

#?(call 'append '(append '(1 2) (let((a 0))(append (list a) '(3 4))) '(5 6)) 'optimize)
=> (let((a 0))
     (append '(1 2) (list a) '(3 4)'(5 6)))
,:test jingoh.tester:sexp=

(requirements-about mapcar :test equal)

#?(call 'mapcar '(mapcar '+ '(1 2 3) ()) 'optimize)
=> NIL

#?(call 'mapcar '(mapcar '+ () (print '(1 2 3))) 'optimize)
=> (progn (print '(1 2 3)) nil)

#?(call 'mapcar '(mapcar #'+ (mapcar #'+ nil (print '(1)))
			 (print '(2))) 'optimize)
=> (mapcar #'+ (progn (print '(1)) nil)
	   (print '(2)))

#?(call 'mapcar '(mapcar (progn #'+) nil) 'optimize)
=> (progn (progn #'+) nil)

(requirements-about list)

#?(call 'list '(list) 'optimize) => NIL

(requirements-about vector :test equalp)

#?(call 'vector '(vector) 'optimize) => #()

(requirements-about concatenate :test equalp)

#?(call 'concatenate '(concatenate 'vector) 'optimize)
=> #()

#?(call 'concatenate '(concatenate 'vector #() (vector)) 'optimize)
=> #()

#?(call 'concatenate '(concatenate (intern "VECTOR") nil "") 'optimize)
=> (coerce nil (intern "VECTOR"))

#?(call 'concatenate '(concatenate 'list (vector)(vector 1 2 3)) 'optimize)
=> (coerce (vector 1 2 3) 'list)

#?(call 'concatenate '(concatenate 'vector #(1) (concatenate 'list #(2) #(3))) 'optimize)
=> (concatenate 'vector #(1) #(2) #(3))

#?(call 'concatenate '(concatenate 'vector '(1) (let((a 0))(concatenate 'list (list a) var)) var2)
	'optimize)
=> (let((a 0))
     (concatenate 'vector '(1) (list a) var var2))
,:test jingoh.tester:sexp=

(requirements-about * :test equal)

#?(call '* '(* 1) 'optimize) => 1

#?(call '* '(* '1) 'optimize) => 1

#?(call '* '(* (the fixnum '1)) 'optimize) => 1

#?(call '* '(* 1 2 (* 3 4 (* 5 6) 7 8) 9 (* 10 11)) 'optimize)
=> (* 2 3 4 5 6 7 8 9 10 11)

#?(call '* '(* 1 2 (* 3 4) 5 (* 6 (* 7 0) 8) 9) 'optimize) => 0

#?(call '* '(* 1 2 (* 3 4) 5 (* 6 (* (print 7) 0) 8) 9) 'optimize)
=> (progn (print 7) 0)

#?(call '* '(* 1 2 (let((a 0))(1+ a)) 3 4) 'optimize)
=> (let((a 0))(* 2 (1+ a) 3 4))
,:test jingoh.tester:sexp=

(requirements-about + :test equal)

#?(call '+ '(+ 0) 'optimize) => 0

#?(call '+ '(+ '0) 'optimize) => 0

#?(call '+ '(+ (the fixnum '0)) 'optimize) => 0

#?(call '+ '(+) 'optimize) => 0

#?(call '+ '(+ (+) (+)) 'optimize) => 0

#?(call '+ '(+ (+ (+ (+)))) 'optimize) => 0

#?(call '+ '(+ (+ (+ (+(print 1))))) 'optimize)
=> (print 1)

#?(call '+ '(+ 1 (let((a 0))(+ a 2 3))) 'optimize)
=> (let((a 0))
     (+ 1 a 2 3))
,:test jingoh.tester:sexp=

(requirements-about optimized-let :test equal)

#?(call 'let '(let()(+ 1 2 3)) 'optimize)
=> (+ 1 2 3)

#?(call 'lst '(let((a var))a) 'optimize)
=> var

(requirements-about optimized-if)

#?(call 'if '(if t 0 1) 'optimize) => 0

#?(call 'if '(if nil 0 1) 'optimize) => 1

#?(call 'if '(if nil 0) 'optimize) => NIL

#?(call 'if '(if (the boolean t) 0 1) 'optimize) => 0

(requirements-about optimized-locally :test equal)

#?(call 'locally '(locally 0) 'optimize) => 0

#?(call 'locally '(locally (declare) 0) 'optimize) => 0

#?(call 'locally '(locally (print var)(print var2)) 'optimize)
=> (progn (print var)(print var2))

(requirements-about optimized-block :test equal)

#?(call 'block '(block tag) 'optimize) => NIL

#?(call 'block '(block tag 0) 'optimize) => 0

#?(call 'block '(block tag (return-from tag 0) (print 1)) 'optimize)
=> 0

#?(call 'block '(block tag (let((a 0))(list a a))) 'optimize)
=> (let((a 0))(list a a))

#?(call 'block '(block tag (print 1)(print 2)) 'optimize)
=> (progn (print 1)(print 2))

#?(call 'block '(block tag (print 1) (return-from tag (print 2)) (print 3))
	'optimize)
=> (block tag (print 1) (return-from tag (print 2)))

#?(call 'block '(block tag (let((a (print 0)))(return-from tag a))) 'optimize)
=> (print 0)

(requirements-about optimized-return-from :test equal)

#?(call 'return-from '(return-from a (return-from b (return-from c var))) 'optimize)
=> (return-from c var)

#?(call 'return-from '(return-from a (let((a 0))(return-from b (list a a)))) 'optimize)
=> (let((a 0))(return-from b (list a a)))
,:test jingoh.tester:sexp=

(requirements-about format :test equal)

#?(call 'format '(format t "~S : ~S" 0 1) 'optimize)
=> (format t #.(macroexpand-1 '(formatter "~S : ~S")) 0 1)

(requirements-about optimized-the :test equal)

#?(call 'the '(the fixnum (the fixnum (the fixnum 0))) 'optimize) => (the fixnum 0)
