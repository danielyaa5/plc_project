

;;; return statement ;;;
;;; returns the given value or the evaluated value of the given calculation.
;;; @param 
(define returnStatement
  (lambda (value)
    (cond
      (((number? value) ; if input is a number, just return that
       value
       ))
      ((pair? (value))
       value
       ); if input is a statement, evaluate that
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

(define booleanOperations
  (lambda (X boolOper Y)
    (cond
      ((eq? boolOper '&&)
       (AND X Y))
      ((eq? boolOper '||)
       (OR X Y))
      (else 'foo ; handle other case?
       ))))
      ; (('!) (NOT X Y))
      

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
  (lambda (X compareOper Y state)
    (cond
      ((eq? compareOper '==) (eq? X Y))
      ((eq? compareOper '>=) (>= X Y))
      ((eq? compareOper '<=) (<= X Y))
      ((eq? compareOper '!=) !(eq? X Y))
      ((eq? compareOper '>) (> X Y))
      ((eq? compareOper '<) (< X Y))
      ; do we need a default case here?
      )))

; #! tests for comparison operators
; (3 >= 3)                  ; returns #t
; (10 == 11)                ; returns #f
; ('foo == 'foo)            ; returns #t
; (10 <= 5)                 ; returns #t
; !#

(define true #t)
(define false #f)

(define getFirstVar caar)
(define getFirstValue cadar)
(define restOf
    (lambda (state)
        (cons (cdar state) (cdadr state))))

;;; Mvalue ;;;
;;; returns the M_value of the expression
(define M_value
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((not (list? expression)) (lookupValue expression state)) ;lookupValue not implemented
      ((eq? 'true) true)
      ((eq? 'false) false)
      ((eq? '+ (operator expression)) (+ (M_value (operand1 expression)) (M_value (operand2 expression))))
      ((eq? '- (operator expression)) (- (M_value (operand1 expression)) (M_value (operand2 expression))))
      ((eq? '* (operator expression)) (* (M_value (operand1 expression)) (M_value (operand2 expression))))
      ((eq? '/ (operator expression)) (quotient (M_value (operand1 expression)) (M_value (operand2 expression))))
      ((eq? '% (operator expression)) (remainder (M_value (operand1 expression)) (M_value (operand2 expression))))
      (else (error 'unknown "unknown expression"))))))
