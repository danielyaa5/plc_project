;;; M_value
;;; calculates the M_value of the given expression and returns it

(define M_value
  (lambda (expr state)
    (cond
      ((eq? 'true expr) true)
      ((eq? 'false expr) false)
      ((null? expr) (error "ERR: variable not yet declared"))
      ((number? expr) expr)
      ((atom? expr) (M_value (M_lookup expr state) state))
      ((and (= (length expr) 2) (eq? '- (operator expr))) (* -1 (M_value (operand1 expr) state)))
      ((eq? '+ (operator expr)) (+ (M_value (operand1 expr) state) 
                                   (M_value (operand2 expr) state)))
      ((eq? '- (operator expr)) (- (M_value (operand1 expr) state)
                                   (M_value (operand2 expr) state)))
      ((eq? '* (operator expr)) (* (M_value (operand1 expr) state) 
                                   (M_value (operand2 expr) state)))
      ((eq? '/ (operator expr)) (quotient (M_value (operand1 expr) state)
                                          (M_value (operand2 expr) state)))
      ((eq? '% (operator expr)) (remainder (M_value (operand1 expr) state) 
                                           (M_value (operand2 expr) state))))))

;;; M_lookup ;;;
;;; looks up the value of the given variable.
;;; If the variable does not exist in state, return an empty list.
(define M_lookup
  (lambda (var state)
    (cond
      ((null? state) '()) ; variable does not exist in state
      ((eq? var (getFirstVar state)) (getFirstValue state))
      (else (M_lookup var (cdr state)))
      )))


(define getFirstVar caar)
(define getFirstValue cadar)
(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))
                 
;;; return statement ;;;
;;; returns the given value or the evaluated value of the given calculation.
;;; @param 
(define returnStatement
  (lambda (expr)
    (cond
      ((number? expr) ; if input is a number, just return that
       expr); if input is a statement, evaluate that
      ((list? expr) ; if input is an expression, then we have to evaluate that
       ))))

;;; condition ;;;
;;; checks what kind of conditional operation the expression is and calls appropriate function
;;; e.g. comparison or boolean

(define M_cond
  (lambda (expr state)
    (cond
      ((number? expr) expr)
      ((eq? 'true expr) true)
      ((eq? 'false expr) false)
      ((eq? (operator expr) '!) (not (M_value (operand1 expr) state)))
      ((or (eq? (operator expr) '&&) (eq? (operator expr) '||))
       (booleanCondition expr state))
      (else (compareCondition expr state))
      )))
            

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
       )))

;;; comparison operators ;;;
;;;
  
(define compareCondition
  (lambda (expr state)
    (cond
      ((eq? (operator expr) '==)
       (eq? (M_value (operand1 expr) state)
            (M_value (operand2 expr) state)))
      ((eq? (operator expr) '!=)
       (not (eq? (M_value (operand1 expr) state)
             (M_value (operand2 expr) state))))
      ((eq? (operator expr) '>=)
       (>= (M_value (operand1 expr) state)
           (M_value (operand2 expr) state)))
      ((eq? (operator expr) '<=)
       (<= (M_value (operand1 expr) state)
           (M_value (operand2 expr) state)))
      ((eq? (operator expr) '>)
       (> (M_value (operand1 expr) state)
          (M_value (operand2 expr) state)))
      ((eq? (operator expr) '<)
       (< (M_value (operand1 expr) state)
          (M_value (operand2 expr) state)))
      (else (error "invalid condition input"))
      )))

; #! tests for comparison operators
; (3 >= 3)                  ; returns #t
; (10 == 11)                ; returns #f
; ('foo == 'foo)            ; returns #t
; (10 <= 5)                 ; returns #t
; !#


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
