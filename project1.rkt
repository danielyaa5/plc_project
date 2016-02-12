

;;; return statement ;;;
;;; returns the given value or the evaluated value of the given calculation.
;;; @param 
(define returnStatement
  (lambda (expr)
    (cond
      ((number? expr) ; if input is a number, just return that
       expr
       'null)
      ((pair? expr)
       expr
       ); if input is a statement, evaluate that
      ((expression? (expr))
       0
       )
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
;;; assuming that format would be (|| X Y); where X Y are operands and || is the operator.

(define booleanCondition
  (lambda (expr state)
    (cond
      ((eq? (operator expr) '&&)
       (and (booleanCondition (operand1 expr) state)
            (booleanCondition (operand2 expr) state)))
      ((eq? (operator expr) '||)
       (or (booleanCondition (operand1 expr) state)
           (booleanCondition (operand2 expr) state)))
      (else 'null); handle other case?
       )))
      ; (('!) (NOT X Y))

;;; comparison operators ;;;
;;; X <comparisonOper> Y, where <comparisonOper> -> ==, !=, <, >, <=. >=
;;; X, Y: the expressions to be evaluated
(define compareConditional
  (lambda (expr state)
    (cond
      ((eq? (operator expr) '==)
       (eq? (compareConditional (operand1 expr) state)
            (compareConditional (operand2 expr) state)))
      ((eq? (operator expr) '>=)
       (>= (compareConditional (operand1 expr) state)
           (compareConditional (operand2 expr) state)))
      ((eq? (operator expr) '<=)
       (<= (compareConditional (operand1 expr) state)
           (compareConditional (operand2 expr) state)))
      ((eq? (operator expr) '!=)
       (not (eq? (compareConditional (operand1 expr) state)
                 (compareConditional (operand2 expr) state))))
      ((eq? (operator expr) '>)
       (> (compareConditional (operand1 expr) state)
          (compareConditional (operand2 expr) state)))
      ((eq? (operator expr) '<)
       (< (compareConditional (operand1 expr) state)
          (compareConditional (operand2 expr) state)))
      ; do we need a default case here?
      )))

; #! tests for comparison operators
; (3 >= 3)                  ; returns #t
; (10 == 11)                ; returns #f
; ('foo == 'foo)            ; returns #t
; (10 <= 5)                 ; returns #t
; !#

;;; some other stuff
(define true #t)
(define false #f)

;;; not sure if we'll need these
(define getFirstVar caar)
(define getFirstValue cadar)
(define restOf
    (lambda (state)
        (cons (cdar state) (cdadr state))))

;;; operator and operands functions ;;;
;; @param expr the expression to find its operator/operands
;; @returns the operator/operand of given expression
;
; operator: returns the operator of the given expression;   usage: (operator ('= x 5)) => '=
; operand1: returns the left-hand operand of the given expression; (operand1 ('= x 5)) => x
; operand2: returns the righthand operand of the given expression; (operand2 ('= x 5)) => 5
(define operator
  (lambda (expr)
    (car expr)))

(define operand1
  (lambda (expr)
    (cadr expr)))

(define operand2
  (lambda (expr)
    (caddr expr)))
