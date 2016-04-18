;;; Hun Jae Lee, Daniel Yakobian, Justin Wang
;;; EECS345 Programming Language Concepts, Project 3
;;; Due Mar 30, 2016 @ Midnight

; Load the parser
; (load "simpleParser.scm")   ; obsolete
; (load "functionParser.scm") ; obsolete
(load "classParser.scm")

; (define call/cc call-with-current-continuation)

;;; interpret ;;;
;; interprets the given file.
;; @filename the name of the file to interpret
(define interpret
  (lambda (filename)
    (interpret-func-call 'main '() (interpret-global-statement-list (parser filename) newstate))))

;;; interpret-global-statement-list ;;;
;; interprets global statements
;; @parsetree the parsed tree from parser
;; @state the state
(define interpret-global-statement-list
  (lambda (parsetree state)
    (cond
     ((null? parsetree) state)
     (else (interpret-global-statement-list (cdr parsetree) (interpret-global-statement (car parsetree) state))))))

;;; interpret-global-statement ;;;
;; interprets one global statement
;; @param statement the statement to interpret
;; @state the state
(define interpret-global-statement
  (lambda (statement state)
    (cond
     ((eq? 'function (car statement)) (interpret-func-declare statement state))
     (else (interpret-statement statement state undef-return undef-break undef-continue undef-throw)))))

;;; interpret-func-declare ;;;
;; interprets function declaration
;; @statement the statement that declares a function
;; @state the state
(define interpret-func-declare
  (lambda (statement state)
    (state-bind (cadr statement) (cddr statement) state)))

;;; interpret-statement-list ;;;
;; interprets a list of statements
;; @parsetree the parsed tree from the parser
;; @state the state
;; @return what to return
;; @break what to break
;; @continue what to continue
;; @throw what to throw
(define interpret-statement-list
  (lambda (parsetree state return break continue throw)
    (cond
     ((null? parsetree) state)
     (else (interpret-statement-list (cdr parsetree) 
        (interpret-statement (car parsetree) state return break continue throw)
        return break continue throw)))))

;;; interpret-func-call ;;;
;; interprets a function call (funcall)
;; @func-name the name of the called function
;; @values the values (parameters) fed into the function
;; @state the state
(define interpret-func-call
  (lambda (func-name values state)
    (cond
     ;test if args === params list
     ((eq? #f (hasCorrectArgs (car (state-lookup func-name state)) values)) error "Incorrect arguments supplied")
     ((eq? 'void (state-lookup 'return (return-state func-name values state))) (state-pop-frame (return-state func-name values state)))
     (else (state-lookup 'return (return-state func-name values state))))))

;;; hasCorrectArgs ;;;
;; checks whether two args are equivalueent.
;; @l1 the list to compare
;; @l2 the list to compare
(define hasCorrectArgs
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((null? l1) #f)
      ((null? l2) #f)
      (else (hasCorrectArgs (cdr l1) (cdr l2))))))
  
;;; return-state ;;;
;; returns the state after a function
;; @func-name the name of the function
;; @values the parameters fed into the function
;; @state the state
(define return-state
  (lambda (func-name values state)
    (call/cc
     (lambda (return)
       (interpret-statement-list (cadr (state-lookup func-name state))
          (create-func-state (car (state-lookup func-name state)) (interpret-called-values values state) state)
          return undef-break undef-continue undef-throw)))))

;;; interpret-called-values ;;;
;; returns the called value
;; @values the list of values
;; @state the state
(define interpret-called-values
  (lambda (values state)
    (cond
     ((null? values) '())
     (else (cons (interpret-value (car values) state) (interpret-called-values (cdr values) state))))))

;;; interpret-statement ;;;
;; interprets a statement.
;; @statement the statement to be interpreted
;; @state the state
;; @return what to return
;; @break what to break
;; @continue what to continue
;; @throw what to throw
(define interpret-statement
  (lambda (statement state return break continue throw)
    (cond
     ((eq? '= (car statement))        (interpret-assign    statement state                       throw))
     ((eq? 'var (car statement))      (interpret-variable  statement state))
     ((eq? 'if (car statement))       (interpret-if        statement state return break continue throw))
     ((eq? 'while (car statement))    (interpret-while     statement state return                throw))
     ((eq? 'break (car statement))    (break                         state))
     ((eq? 'continue (car statement)) (continue                      state))
     ((eq? 'begin (car statement))    (interpret-begin     statement state return break continue throw))
     ((eq? 'try (car statement))      (interpret-try (cadr statement) (caddr statement) (cadddr statement)
                                                                     state return continue break throw))
     ((eq? 'return (car statement))   (return (interpret-return statement state)))
     ((eq? 'funcall (car statement))  (other-func-call (cadr statement) (cddr statement) state))
     ((eq? 'function (car statement)) (interpret-func-declare statement state))
     ((eq? 'throw (car statement)) (throw (cdr statement)))
     (else (error "Error: Not a valueid statement")))))


(define other-func-call
  (lambda (func-name values state)
    (cond
     ((eq? #f (hasCorrectArgs (car (state-lookup func-name state)) values)) error "Incorrect arguments supplied")
     ((eq? 'void (state-lookup 'return (return-state func-name values state))) (state-pop-frame (return-state func-name values state)))
     (else (return-state func-name values state)))))

;;; interpret-sideEffects ;;;
;; interprets a statement, and prevent side effects.
;; @statement the statement to be interpreted
;; @state the state
(define interpret-sideEffects
  (lambda (statement state)
    (cond
     ((number? statement) state)
     ((atom? statement) state)
     ((eq? '! (car statement)) (interpret-sideEffects (LHS statement) state))
     ((and (eq? '- (car statement)) (eq? 2 (length statement))) (interpret-sideEffects (LHS statement) state))
     ((member? (car statement) '(+ - * / % == != > >= < <= && ||)) 
      (interpret-sideEffects (RHS statement) (interpret-sideEffects (LHS statement) state)))
     ((eq? 'begin (car statement)) state)
     ((eq? 'break (car statement)) state)
     ((eq? 'funcall (car statement)) state)
     (else (interpret-statement statement state undef-return undef-break undef-continue)))))

;;; interpret-begin ;;;
;; interprets a block
;; @statement the statement to be interpreted.
;; @state the state
;; @return what to return
;; @break what to break
;; @continue what to continue
;; @throw what to throw
(define interpret-begin
  (lambda (statement state return break continue throw)
    (let ((pop-break (lambda (break-state) (break (state-pop-frame break-state))))
    (pop-continue (lambda (continue-state) (continue (state-pop-frame continue-state)))))
      (state-pop-frame (interpret-statement-list (cdr statement) (state-push-frame state) return pop-break pop-continue throw)))))

;;; interpret-while ;;;
;; interprets a while statement
;; @statement the statement to be interpreted.
;; @state the state
;; @return what to return
;; @throw what to throw
(define interpret-while
  (lambda (statement state return throw)
    (call/cc (lambda (break)
         (letrec ((loop (lambda (condition body state)
        (cond
         ((eq? (interpret-value condition state) 'true) 
          (loop condition body (call/cc (lambda (continue)
                  (interpret-statement body (interpret-sideEffects condition state) return break continue throw)))))
         (else (interpret-sideEffects condition state))))))
     (loop (cadr statement) (caddr statement) state))))))

;;; interpret-assign ;;;
;; interprets an assignment statement
;; @statement assign statement
;; @state the state
;; @throw what to throw
(define interpret-assign
  (lambda (statement state throw)
    (state-update (LHS statement) (interpret-value (RHS statement) state) (interpret-sideEffects (RHS statement) state))))
  
;;; interpret-variable ;;;
;; sets a new variable
;; @statement variable statement
;; @state the state
(define interpret-variable
  (lambda (statement state)
    (cond
     ((in-frame? (LHS statement) (top-frame state)) (error "Error: Cant redeclare variableiables"))
     ((null? (cddr statement)) (state-bind (LHS statement) 'NEWVAR state))
     (else (state-bind (LHS statement) (interpret-value (RHS statement) state) (interpret-sideEffects (RHS statement) state))))))
  
;;; interpret-return ;;;
;; interprets the return statement
;; @statement the return statement
;; @state the state
(define interpret-return
  (lambda (statement state)
    (cond
     ((and (pair? (cadr statement)) (eq? 'funcall (caadr statement))) (state-bind 'return (interpret-func-call (cadadr statement) (cddadr statement) state) state))
    (else (state-bind 'return (interpret-value (LHS statement) state) (interpret-sideEffects (LHS statement) state))))))

;;; interpret-if ;;;
;; interprets an if-statement
;; @statement the if-statement
;; @state the state
;; @return what to return
;; @break what to break
;; @continue what to continue
;; @throw what to throw
(define interpret-if
  (lambda (statement state return break continue throw)
    (cond
     ((eq? (interpret-value (cadr statement) state) 'true) 
      (interpret-statement (caddr statement) (interpret-sideEffects (cadr statement) state) return break continue throw))
     ((interpret-else? statement) 
      (interpret-statement (cadddr statement) (interpret-sideEffects (cadr statement) state) return break continue throw))
     (else state))))
     
;;; interpret-else? ;;;
;; interprets the optional else statement
;; @statement the list of optional else statement
(define interpret-else?
  (lambda (statement)
    (cond
     ((null? (cdddr statement)) #f)
     (else #t))))

;;; interpret-try ;;;
;; interprets the try-body.
;; @tryBody the body of the try statement
;; @catch the catch-body of the statement
;; @finally the finally-body of the statement
;; @state the state
;; @return what to return
;; @break what to break
;; @continue what to continue
;; @throw what to throw
(define interpret-try
  (lambda (tryBody catch finally state return break continue throw)
    (cond
      ((null? tryBody) (interpret-statement-list (car (cdr finally)) state return break continue throw))
      ((eq? 'throw (car (car tryBody))) (interpret-catch (caddr catch) finally (caadr catch) (interpret-value (cdar tryBody) state) state return break continue throw))
      (else (interpret-try (cdr tryBody) catch finally (interpret-statement (car tryBody) state return break continue throw) return break continue throw))
      )))

;;; interpret-catch ;;;
;; interprets the catch-body.
;; @catchBody the body of the catch statement
;; @finally the finally-body of the statement
;; @nameOfException the name of the exception
;; @e the exception
;; @state the state
;; @return what to return
;; @break what to break
;; @continue what to continue
;; @throw what to throw
(define interpret-catch
  (lambda (catchBody finally nameOfException e state return break continue throw)
    (cond
      ((not (null? e)) (interpret-catch catchBody finally nameOfException null (assign nameOfException e state) return break continue throw))
      ((null? catchBody) (interpret_statements (cadr finally) state return break continue))
      (else (interpret-catch (cdr catchBody) finally nameOfException null (interpret-statement (car catchBody) state return break continue) return break continue throw))
      )))

(define another-func-call
  (lambda (env)
    (state-lookup 'return env)))

;;; interpret-value ;;;
;; interprets the value
;; @statement the statement that calls this value.
;; @state the state.
(define interpret-value
  (lambda (statement state)
    (cond
     ((null? statement) '())
     ((number? statement) statement)
     ((eq? statement 'true) 'true)
     ((eq? statement 'false) 'false)
     ((atom? statement) (state-lookup statement state))
     ((eq? 'funcall (operator statement)) (interpret-value '(another-func-call) (other-func-call (cadr statement) (cddr statement) state)))
     ((eq? 'another-func-call (operator statement)) (another-func-call state))
     ((eq? '= (operator statement)) (interpret-value (car (cddr statement)) state))
     ((eq? '+ (operator statement)) ((interpret-binary +)         statement state))
     ((eq? '- (operator statement)) ((interpret-negative -)       statement state))
     ((eq? '* (operator statement)) ((interpret-binary *)         statement state))
     ((eq? '/ (operator statement)) ((interpret-binary quotient)  statement state))
     ((eq? '% (operator statement)) ((interpret-binary remainder) statement state))
     ((eq? '> (operator statement)) ((interpret-boolean >)        statement state))
     ((eq? '< (operator statement)) ((interpret-boolean <)        statement state))
     ((eq? '>= (operator statement)) ((interpret-boolean >=)      statement state))
     ((eq? '<= (operator statement)) ((interpret-boolean <=)      statement state))
     ((eq? '!= (operator statement)) ((interpret-boolean (lambda (a b) (not (eq? a b)))) statement state))
     ((eq? '== (operator statement)) ((interpret-boolean (lambda (a b) (eq? a b))) statement state))
     ((eq? '|| (operator statement)) ((interpret-boolean (lambda (a b)
                  (cond
                   ((and (eq? a 'true) (eq? b 'true)) #t)
                   ((and (eq? a 'true) (eq? b 'false)) #t)
                   ((and (eq? a 'false) (eq? b 'true)) #t)
                   ((and (eq? a 'false) (eq? b 'false)) #f))))
         statement state))
     ((eq? '&& (operator statement)) ((interpret-boolean (lambda (a b)
                   (cond
                   ((and (eq? a 'true) (eq? b 'true)) #t)
                   ((and (eq? a 'true) (eq? b 'false)) #f)
                   ((and (eq? a 'false) (eq? b 'true)) #f)
                   ((and (eq? a 'false) (eq? b 'false)) #f))))
         statement state))
     ((eq? '! (operator statement)) ((interpret-unary-boolean (lambda (a) (cond ((eq? a 'true) #f)
                     ((eq? a 'false) #t))))
        statement state))
     (else (error "Invalueid expressionession")))))

;;; interpret-unary-boolean ;;;
;; interprets the unary boolean (!true)
;; @op the operator
(define interpret-unary-boolean
  (lambda (op)
    (lambda (statement state)
      (cond
       ((op (interpret-value (operand1 statement) state)) 'true)
       (else 'false)))))

;;; interpret-boolean ;;;
;; interprets a boolean statement
;; @op the boolean operator
(define interpret-boolean
  (lambda (op)
    (lambda (statement state)
      (cond
       ((op (interpret-value (operand1 statement) state) (interpret-value (operand2 statement) (interpret-sideEffects (operand1 statement) state))) 'true)
       (else 'false)))))
      
;;; interpret-binary ;;;
;; interprets a binary statement
;; @op the operator
(define interpret-binary
  (lambda (op)
    (lambda (statement state)
      (op (interpret-value (operand1 statement) state)
    (interpret-value (operand2 statement) (interpret-sideEffects (operand1 statement) state))))))

;;; interpret-negative ;;;
;; interprets a negative number (-1)
;; @op the operator
(define interpret-negative
  (lambda (op)
    (lambda (statement state)
      (cond
       ((null? (cddr statement)) (* -1 (interpret-value (operand1 statement) state)))
       (else (op (interpret-value (operand1 statement) state)
     (interpret-value (operand2 statement) state)))))))

;; abstractions
(define operator (lambda (expression) (car expression)))
(define operand1 (lambda (expression) (car (cdr expression))))
(define operand2 (lambda (expression) (car (cdr (cdr expression)))))
(define LHS (lambda (statement) (car (cdr statement))))
(define RHS (lambda (statement) (car (cdr (cdr statement)))))
(define atom? (lambda (statement) (not (or (pair? statement) (null? statement)))))


;;; member? ;;;
;; checks whether a is in l
;; @a the element to check in the list
;; @l the list to check
(define member?
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((eq? (car l) a) #t)
     (else (member? a (cdr l))))))

;;; undef-return ;;;
;; returns a user-friendly error message that return cannot be used in this context
;; @statement the return statement
(define undef-return
  (lambda (statement)
    (error "Return cannot be used in this context")))

;;; undef-break ;;;
;; returns a user-friendly error message that break cannot be used in this context
;; @statement the break statement
(define undef-break
  (lambda (statement)
    (error "Break cannot be used in this context")))

;;; undef-continue ;;;
;; returns a user-friendly error message that continue cannot be used in this context
;; @statement the continue statement
(define undef-continue
  (lambda (statement)
    (error "Continue cannot be used in this context")))

;;; undef-throw ;;;
;; returns a user-friendly error message that throw cannot be used in this context
;; @statement the throw statement
(define undef-throw
  (lambda (statement)
    (error "Exception thrown")))


; abstraction of a new state ;
(define newstate '(((return) (void))))

;;; create-func-state ;;;
;; creates a function state
;; @formal-params the formal parameters for this state.
;; @values the values
;; @state the state
(define create-func-state
  (lambda (formal-params values state)
    (letrec ((add-bindings 
        (lambda (formal values state)
    (cond
     ((null? formal) state)
     (else (add-bindings (cdr formal) (cdr values) (state-bind (car formal) (car values) state)))))))
      (add-bindings formal-params values (state-push-frame (state-global-frame state))))))

;;; state-global-frame ;;;
;; the global-level frame of state
;; @state the state
(define state-global-frame
  (lambda (state)
    (cond
     ((null? (cdr state)) state)
     (state-global-frame (cdr state)))))

;;; state-push-frame ;;;
;; pushes a new frame into state
;; @state the state
(define state-push-frame
  (lambda (state)
    (cons '(() ()) state)))

;;; state-pop-frame ;;;
;; pops a frame from state
;; @state the state
(define state-pop-frame
  (lambda (state)
    (cdr state)))

;;; in-frame? ;;;
;; checks whether a variable is in the frame
;; @variable the variable to check whether it is in the frame
;; @frame the frame to check the variable in
(define in-frame?
  (lambda (variable frame)
    (cond
     ((null? (top-variable frame)) #f)
     ((eq? variable (top-variable frame)) #t)
     (else (in-frame? variable (frame-pop-top frame))))))

;;; state-lookup ;;;
;; looks up the given variable in the state
;; @variable the variable to look up
;; @state the state
(define state-lookup
  (lambda (variable state)
    (cond
     ((null? state) (error "Error: Variable not in state"))
     ((in-frame? variable (top-frame state)) (state-lookup-frame variable (top-frame state)))
     (else (state-lookup variable (state-pop-frame state))))))

;;; state-lookup-frame ;;;
;; looks up the given frame in the state
;; @frame the frame to look up
;; @state the state
(define state-lookup-frame
  (lambda (variable frame)
    (cond
     ((null? (top-variable frame)) (top-variable frame))
     ((eq? (top-variable frame) variable) (top-value frame))
     (else (state-lookup-frame variable (frame-pop-top frame))))))

;;; state-bind ;;;
;; binds to the state
;; @variable the variable to bind
;; @value the value of that variable
;; @state the state
(define state-bind
  (lambda (variable value state)
      (cons (cons (cons variable (car (top-frame state))) 
                  (cons (cons value (cadr (top-frame state))) '()))
            (state-pop-frame state))))

;;; state-update ;;;
;; updates the state
;; @variable the variable to update the value of
;; @value the new value
;; @state the state
(define state-update
  (lambda (variable value state)
    (letrec ((state-update-checked 
        (lambda (variable value state)
    (cond
     ((in-frame? variable (top-frame state)) (state-bind variable value state))
     (else (cons (top-frame state) (state-update-checked variable value (state-pop-frame state))))))))
      (cond
       ((not (state-declared? variable state)) (error "Error: Variable not declared"))
       (else (state-update-checked variable value state))))))

;;; state-declared? ;;;
;; checks whether the variable is declared in the state
;; @variable the variable to check whether it is declared in the state
;; @state the state
(define state-declared?
  (lambda (variable state)
    (cond
     ((null? state) #f)
     ((in-frame? variable (top-frame state)) #t)
     (else (state-declared? variable (state-pop-frame state))))))

;;; top-variable ;;;
;; abstraction, refers to the top of the frame
;; @frame the frame to check its top of
(define top-variable
  (lambda (frame)
    (cond
     ((null? (car frame)) '())
     (else (car (car frame))))))

;;; top-value ;;;
;; abstraction, refers to the top of the frame
;; @frame the frame to check its top of
(define top-value
  (lambda (frame)
    (cond
     ((null? (cadr frame)) '())
     (else (caadr frame)))))

;;; top-frame ;;;
;; the topmost frame
;; @state the state
(define top-frame
  (lambda (state)
    (car state)))

;;; frame-pop-top ;;;
;; pops the topmost frame from the frame
;; @frame the frame
(define frame-pop-top
  (lambda (frame)
    (cons (cdar frame) (cons (cdadr frame) '()))))

(parser "test/test11")
(interpret "test/test15")