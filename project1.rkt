;;; return statement ;;;
;;; returns the given value or the evaluated value of the given calculation.
;;; @param 
(define return
  (lambda (value)
    (cond
      (((number? value) ; if input is a number, just return that
       value
       ))
      ;((pair? (value) ; if input is a evaluation, run evaluation method?
      (else
       0
       )               ; not sure how to handle other cases
      )))

;;; tests are disabled until functions are complete
;#! tests for return
;(return 150)              ; returns 150
;(return 3 * 4)            ; returns 12 (evaluates 3 * 4)
;(return 'hello)           ; returns the string "hello"
;; assume x = 3, y = 5 assigned
;(return x + y)            ; returns 8
;!#

;;; boolean operators ;;;
;;; X <boolOper> Y, where <boolOper> -> &&, ||, ! (but ! is a different case)
;;; X, Y: the expressions to be evaluated

(define bool
  (lambda (X boolOper Y)
    (case boolOper
      ('&& (AND X Y))
      ('|| (OR X Y))
      ; (('!) (NOT X Y))
      )))

; #! tests for boolean operators
; (#t || #f)                ; returns #t
; (#t && #f)                ; returns #f
; (#f || #f)                ; returns #f
; (!#t)                     ; returns #f
; !#


;;; comparison operators ;;;
;;; X <comparisonOper> Y, where <comparisonOper> -> ==, !=, <, >, <=. >=
;;; X, Y: the expressions to be evaluated
(define compare
  (lambda (X compareOper Y)
    (case compareOper
      (('==) (eq? X Y))
      (('>=) (>= X Y))
      (('<=) (<= X Y))
      (('!=) !(eq? X Y))
      (('>) (> X Y))
      (('<) (< X Y))
      ; do we need a default case here?
      )))

; #! tests for comparison operators
; (3 >= 3)                  ; returns #t
; (10 == 11)                ; returns #f
; ('foo == 'foo)            ; returns #t
; (10 <= 5)                 ; returns #t
; !#
