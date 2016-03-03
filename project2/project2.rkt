(load "simpleParser.scm")

;;; interpret ;;;
;;; reads in the file (test suite) and runs it.
(define interpret
  (lambda (filename)
    (call/cc (lambda (return)
               (go (parser filename)
                   '()
                   return
                   (lambda (v) (error "Unaccpetable break exception"))
                   (lambda (v) (error "Unacceptable continue exception"))
                   )))))

;;; go ;;;
;;; the upper-level logic. reads the entire list of statements and calls run on each.
;;; when done, returns the state.
(define go
  (lambda (stmts state return break continue)
    (if (null? stmts)
        state
        (go (cdr stmts) (run (car stmts) state return break continue) return break continue)
        )))


;;; run ;;;
;;; the upper-level logic. reads each line and runs it
(define run
  (lambda (stmt state return break continue)
    (cond
      ((eq? 'return  stmt) (return             (M_value (cadr stmt) state)))
      ((eq? 'while   stmt) (whileStatement     (restOf stmt) state return))
      ((and (eq? 'if stmt) (null? (cdddr stmt)))
                                 (ifStatement        (cadr stmt) (caddr stmt)
                                                                   state return break continue))
      ((eq? 'if      stmt) (if-elseStatement   (cadr stmt) (caddr stmt) (cadddr expr) 
                                                                   state return break continue))
      ((eq? 'while   stmt) (whileStatement     (restOf stmt) state return))
      ((eq? 'var     stmt) (declareStatement   (restOf stmt) state))
      ((eq? '=       stmt) (assignStatement    (restOf stmt) state))
      ((eq? 'begin   stmt) (beginBlock         (restOf stmt) state return break continue))
      ((eq? 'break   stmt) (break                            state))
      ((eq? 'continue stmt) (continue                        state))
      ((eq? 'throw   stmt) (throw                            state))
      (else (M_value (restOf stmt) state)) ; I'm not sure if we need this???
      ))) ; for assignStatement we need the first one (e.g. we need the x in x = 5)

;;; M_state-add ;;;
;;; Adds a value into the M_state
(define M_state-add
  (lambda (variable value state)
    (cond
      ((null? value) (cons (cons variable (cons 'UNDEFINED '())) state))
      (else (cons (cons variable (cons (M_value value state) '())) state)))))


;;; M_state-update ;;;
;;; updates a value in the M_state
;;; a non-existing value in the M_state (e.g. running x = 3 when there is only (y 5) in state) should throw an error as well.
(define M_state-update
  (lambda (variable value state)
    (cond
      ((eq? (car (car state)) variable) (M_state-add variable value (cdr state)))
      ((null? (M_lookup variable state)) (error "undeclared variable"))
      (else (cons (car state) (M_state-update variable value (cdr state)))))))

;;; M_value ;;;
;;; calculates the M_value of the given expression and returns it
(define M_value
  (lambda (expr state)
    (cond
      ((null? expr) '())
      ((number? expr) expr)
      ((atom? expr) (M_value (M_lookup expr state) state))
      ((and (eq? (length expr) 2) (eq? '- (operator expr))) (* -1 (M_value (operand1 expr) state)))
      ((eq? '= (operator expr)) (assignStatement (restOf expr) state))
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
      (else (M_cond expr state))
      ;(else (error "not recognized"))
      )))

;;; M_lookup ;;;
;;; looks up the value of the given variable.
;;; If the variable does not exist in state, return false (#f)
(define M_lookup
  (lambda (var state)
    (cond
      ((null? state) #f) ; variable does not exist in state
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
      ((eq? 'true expr) 'true)
      ((eq? 'false expr) 'false)
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
      ((and (atom? (cadr stmt)) (null? (M_lookup (cadr stmt) state))) (error "undeclared variable"))
      ((eq? 'UNDEFINED (M_lookup (operator stmt) state)) (M_state-update (operator stmt) (M_value (cadr stmt) state) state))
      ((atom? (M_lookup (operator stmt) state)) (M_state-update (operator stmt) (M_value (cadr stmt) state) state))
      
      (else (M_state-add (operator stmt) (M_value (cadr stmt) state) state))
      
      ))) ;;; undef

;;; declare statement ;;; e.g. (x), (x 3), or even (x (+ 3 5))
(define declareStatement
  (lambda (stmt state)
    (cond
      ((null? (cdr stmt)) (M_state-add (car stmt) '() state)) ;; undefe
      ((eq? (M_lookup (car stmt) state) 'UNDEFINED) (M_state-update (car stmt) (cdr stmt)))
      (else (M_state-add (car stmt) (M_value (cadr stmt) state) state))
      )))

;;; if statement ;;;
;;; if <cond> <stmt> <elseStmt>
(define ifStatement
  (lambda (condition stmt state return break continue)
    (if (eq? (M_cond condition state) true)
        (M_value stmt state) state
        )))

(define if-elseStatement
  (lambda (condition stmt elseStmt state return break continue)
    (cond
      ((eq? (M_cond condition state) #t) (M_value stmt state return break continue))
      (else (M_value elseStmt state))
      )))

;;; while statement ;;;
;;; while <cond> <stmt>
(define whileStatement
  (lambda (stmt state return)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (cond body state)
                                (if (M_value cond state)
                                    (loop cond body (call/cc (lambda (continue) (go body state return break continue))))
                                    state))))
                 (loop (car stmt) (cadr stmt) state)))
             )))


;;; boolean operators ;;;
;;; assuming that format would be (|| X Y); where X Y are operands and || is the operator.
(define booleanCondition
  (lambda (expr state)
    (cond
      ((eq? (operator expr) '&&)
       (and (M_cond (operand1 expr) state)
            (M_cond (operand2 expr) state)))
      ((eq? (operator expr) '||)
       (or (M_cond (operand1 expr) state)
           (M_cond (operand2 expr) state)))
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
      )))


;;; beginBlock ;;;
;;; interprets a block
(define beginBlock
  (lambda (stmt state return break cont)
    (run (cdr stmt)
         (return state break cont)
         (return)
         (lambda (v) (break (return v)))
         (lambda (v) (cont (return v)))
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


;;; quicktest, commented out.
(parser "test/test1")
(go (parser "test/test1") '() '() '() '())


