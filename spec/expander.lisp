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

(requirements-about DEFEXPANDTABLE)

;;;; Description:

#+syntax
(DEFEXPANDTABLE name &rest clause) ; => result

;;;; Arguments and Values:

; name := 

; clause := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about *EXPANDTABLE*)

;;;; Description:

;;;; Value type is HASH-TABLE
;#? *EXPANDTABLE* :be-the ???

; Initial value is #<HASH-TABLE :TEST EQ :COUNT 28 {9A2F4001}>

;;;; Affected By:

;;;; Notes:

(requirements-about EXPANDER-ERROR)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; expander-error error serious-condition condition slot-object t

;;;; Effective Slots:

;;;; Notes:

(requirements-about MISSING-EXPANDTABLE)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; missing-expandtable expander-error cell-error error serious-condition condition slot-object t

;;;; Effective Slots:

; NAME [Type] T
; [READER] cell-error-name

;;;; Notes:

(requirements-about EXPANDER-CONFLICT)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; expander-conflict expander-error cell-error error serious-condition condition slot-object t

;;;; Effective Slots:

; NAME [Type] T
; [READER] cell-error-name

;;;; Notes:

(requirements-about USE-PREV)

;;;; Description:

#+syntax
(USE-PREV condition) ; => result

;;;; Arguments and Values:

; condition := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about USE-NEXT)

;;;; Description:

#+syntax
(USE-NEXT condition) ; => result

;;;; Arguments and Values:

; condition := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FIND-EXPANDTABLE)

;;;; Description:

#+syntax
(FIND-EXPANDTABLE name &optional errorp) ; => result

;;;; Arguments and Values:

; name := 

; errorp := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MAKE-EXPANDTABLE)

;;;; Description:

#+syntax
(MAKE-EXPANDTABLE clauses) ; => result

;;;; Arguments and Values:

; clauses := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

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

