(load "simpleParser.scm")

;;; interpret
(define interpret
  (lambda (filename)
    (go (parser filename) '())))


;;; go ;;;
;;; the upper-level logic. reads the entire list of statements and calls run on each.
;;; when done, returns the state.
(define go
  (lambda (stmts state)
    (if (not (null? stmts))         
        (go (cdr stmts) (run (car stmts) state))
        (M_value 'return state)
     )))


;;; run ;;;
;;; the upper-level logic. reads each line and runs it
(define run
  (lambda (expr state)
    (cond
      ((eq? 'return (car expr)) (returnStatement (cadr expr) state))
      ((eq? 'while (car expr)) (whileStatement (restOf expr) state))
      ((eq? 'if (car expr)) (ifStatement (restOf expr) state))
      ((eq? 'while (car expr)) (whileStatement (restOf expr) state))
      ((eq? 'var (car expr)) (declareStatement (restOf expr) state))
      ((eq? (car expr) (M_lookup (car expr) state) (assignStatement expr state)))
      ))) ; for assignStatement we need the first one (e.g. we need the x in x = 5)

;;; M_state-add ;;;
;;; Adds a value into the M_state
(define M_state-add
  (lambda (variable value state)
    (cond
      ((null? value) (cons (cons variable (cons '() '())) state))
      (else (cons (cons variable (M_value value state)) state)))))

;;; M_state-update ;;;
;;; updates a value in the M_state
;;; a non-existing value in the M_state (e.g. running x = 3 when there is only (y 5) in state) should throw an error as well.
(define M_state-update
  (lambda (variable value state)
    (cond
      ((eq? (car (car state)) variable) (M_state-add variable value (cdr state)))
      ((null? (M_lookup variable state) (error "undeclared variable")))
      (else (cons (car state) (M_state-update variable value (cdr state)))))))

;;; M_value ;;;
;;; calculates the M_value of the given expression and returns it
(define M_value
  (lambda (expr state)
    (cond
      ((eq? 'true expr) true)
      ((eq? 'false expr) false)
      ((number? expr) expr)
      ((atom? expr) (M_value (M_lookup expr state) state))
      ((and (eq? (length expr) 2) (eq? '- (operator expr))) (* -1 (M_value (operand1 expr) state)))
      ((eq? '= (operator expr)) (M_state-add (operand1 expr) (operand2 expr) state))
      ((eq? '+ (operator expr)) (+ (M_value (operand1 expr) state) 
                                   (M_value (operand2 expr) state)))
      ((eq? '- (operator expr)) (- (M_value (operand1 expr) state)
                                   (M_value (operand2 expr) state)))
      ((eq? '* (operator expr)) (* (M_value (operand1 expr) state) 
                                   (M_value (operand2 expr) state)))
      ((eq? '/ (operator expr)) (quotient (M_value (operand1 expr) state)
                                          (M_value (operand2 expr) state)))
      ((eq? '% (operator expr)) (remainder (M_value (operand1 expr) state) 
                                           (M_value (operand2 expr) state)))
      
      ;(else (error "not recognized"))
      )))

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

;;; miscellaneous helper methods ;;;
(define getFirstVar caar)
(define getFirstValue cadar)
(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))
(define restOf cdr)
                 
;;; return statement ;;;
;;; returns the given value or the evaluated value of the given calculation.
;;; @param 
(define returnStatement
  (lambda (expr state)
    (M_state-add 'return expr state)))

;;; assign statement ;;; e.g. (x 3)
(define assignStatement
  (lambda (stmt state)
    (cond
      ((null? (M_lookup (operator stmt) state)) M_state-add (operator stmt) (M_value (cadr stmt) state) state)
      ((eq? 'UNDEFINED (M_lookup (operator stmt) state)) (M_state-update (operator stmt) (M_value (cadr stmt) state) state)
            ))))

;;; declare statement ;;; e.g. (x), (x 3), or even (x (+ 3 5))
(define declareStatement
  (lambda (stmt state)
    (cond
      ((null? (cdr stmt)) (M_state-add stmt 'UNDEFINED state))
      ((null? (M_lookup (car stmt) state)) (M_state-add (car stmt) (M_value (cdr stmt) state) state))
     )))
            
;;; if statement ;;;
;;; if <cond> <stmt> <elseStmt>
(define ifStatement
  (lambda (cond stmt elseStmt state)
    ((if (eq? (M_cond cond state) true)
         (M_value stmt state)
         (M_value elseStmt state)
       ))))

;;; while statement ;;;
;;; while <cond> <stmt>
(define whileStatement
  (lambda (cond stmt state)
    ((eq? (M_cond cond state) true) (M_value stmt))))
  
    
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
; operator: returns the operator of the given expression;   usage: (operator ('= x 5)) => '=
; operand1: returns the left-hand operand of the given expression; (operand1 ('= x 5)) => x
; operand2: returns the righthand operand of the given expression; (operand2 ('= x 5)) => 5
(define operator car)
(define operand1 cadr)
(define operand2 caddr)



(parser "test/test2.txt")
(go (parser "test/test2.txt") `())