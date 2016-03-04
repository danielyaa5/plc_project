(load "simpleParser.scm")

;;; interpret ;;;
;;; the topmost-level logic. reads in a file (test suite) and runs it.
;; @filename the name/path of the file.
(define interpret
  (lambda (filename)
    (breakdown
     (call/cc (lambda (return)
      (read_statements
        filename
        (newstate)
        return
        (lambda (v) (error "Unacceptable break statement"))
        (lambda (v) (error "Unacceptable continue statement"))
        ))))))

;;; breakdown ;;;
;;; breaks down a value (ie. true and false)
(define breakdown
  (lambda (value)
    (cond
      ((and (not (number? value)) value)       "true" )
      ((and (not (number? value)) (not value)) "false")
      (else value)
      )))

;;; read_statements ;;;
;;; reads each statement and recursively runs them
;; @statements the list of statements
;; @state the state of current program
;; @return the return function
;; @break what to do for break
;; @continue the continuation
(define read_statements (lambda (statements state return break continue)
    (if (null? statements)
     state
     (read_statements
      (cdr statements)
      (interpret_statement (car statements) state return break continue)
      return
      break
      continue)
     )))

;;; interpret_statement ;;;
;;; reads each statement and do the appropriate thing
;; @stmt the statement
;; @state the state of current program
;; @return the return function
;; @break what to do for break
;; @continue the continuation
(define interpret_statement (lambda (stmt state return break continue)
    (cond
      ((eq? 'return   (keywordOf stmt)) (return (M_value (operand1 stmt) state)            ))
      ((eq? '=        (keywordOf stmt)) (M_assign          stmt state                      ))
      ((eq? 'var      (keywordOf stmt)) (M_declare         stmt state                      ))
      ((eq? 'if       (keywordOf stmt)) (interpret_if      stmt state return break continue))
      ((eq? 'while    (keywordOf stmt)) (interpret_while   stmt state return               ))
      ((eq? 'begin    (keywordOf stmt)) (interpret_begin   stmt state return break continue))
      ((eq? 'break    (keywordOf stmt)) (break                  state                      ))
      ((eq? 'continue (keywordOf stmt)) (continue               state                      ))
      ((eq? 'try      (keywordOf stmt)) (interpret_try     stmt state return break continue))
      (else                             (M_value           stmt state                      ))
      )))

; stmt: ("try" trybody (catch (e) body) finallybody 
(define interpret_try
  (lambda (stmt state return break continue)
    (cond
      ((null? (caddr stmt)) (interpret_begin ((cadr stmt) state return break continue)) ; no finally body
      ((null? (cadr stmt)) (interpret_begin ((cadr stmt) state return break continue)) ; no catch body
                           )))))


;;; M_assign ;;;
;;; interprets an assignment statement and and adds it
;; @stmt the assignment statement
;; @state the state of current program.
(define M_assign (lambda (stmt state)
    (assign (operand1 stmt) (M_value (operand2 stmt) state) state)
    ))

;;; interpret_begin ;;;
;;; interprets a begin (block).
;; @stmt the begin block
;; @state the state of current program
;; @return the return function
;; @break what to do for break
;; @continue the continuation
(define interpret_begin (lambda (stmt state return break continue)
    (popstack
      (read_statements
        (cdr stmt)
        (pushstack state)
        return
        (lambda (v)
          (break (popstack v)))
        (lambda (v)
          (continue (popstack v)))
        ))))

;;; M_declare ;;;
;;; interprets a declaration (both declare and declare+assign)
;; @stmt the declaration statement
;; @state the state of the current program.
(define M_declare (lambda (stmt state)
    (if (null? (operand2 stmt))
      (declare (operand1 stmt) state)
      (assign (operand1 stmt) (M_value (operand2 stmt) state) (declare (operand1 stmt) state))
      )))

;;; interpret_if ;;;
;;; interprets an if statement (both unmatched and matched)
;; @stmt the if statement
;; @state the state of the current program
;; @return the return function
;; @break what to do for break
;; @continue the continuation
(define interpret_if (lambda (stmt state return break continue)
    (cond
      ((M_value (operand1 stmt) state) (interpret_statement (operand2 stmt) state return break continue))
      ((null? (operand3 stmt)) state)
      (else (interpret_statement (operand3 stmt) state return break continue))
      )))

;;; M_value ;;;
;;; interprets the value of the statement/variable.
;; @stmt the statement to interpret
;; @state the state of the current program
(define M_value (lambda (stmt state)
    (cond
      ((null? stmt) '())
      ((number? stmt) stmt)
      ((eq? 'true  stmt) #t)
      ((eq? 'false stmt) #f)
      ((not (list? stmt)) (lookup stmt state))
      ((null? (cdr stmt)) (M_value (car stmt) state))
      ((and (eq? '- (op stmt)) (null? (operand2 stmt))) (-            (M_value (operand1 stmt) state)))
      ((eq? '+  (op stmt)) (+         (M_value (operand1 stmt) state) (M_value (operand2 stmt) state)))
      ((eq? '-  (op stmt)) (-         (M_value (operand1 stmt) state) (M_value (operand2 stmt) state)))
      ((eq? '*  (op stmt)) (*         (M_value (operand1 stmt) state) (M_value (operand2 stmt) state)))
      ((eq? '/  (op stmt)) (quotient  (M_value (operand1 stmt) state) (M_value (operand2 stmt) state)))
      ((eq? '%  (op stmt)) (remainder (M_value (operand1 stmt) state) (M_value (operand2 stmt) state)))
      ((eq? '== (op stmt)) (eq?       (M_value (operand1 stmt) state) (M_value (operand2 stmt) state)))
      ((eq? '!= (op stmt)) (not (=    (M_value (operand1 stmt) state) (M_value (operand2 stmt) state))))
      ((eq? '<  (op stmt)) (<         (M_value (operand1 stmt) state) (M_value (operand2 stmt) state)))
      ((eq? '>  (op stmt)) (>         (M_value (operand1 stmt) state) (M_value (operand2 stmt) state)))
      ((eq? '<= (op stmt)) (<=        (M_value (operand1 stmt) state) (M_value (operand2 stmt) state)))
      ((eq? '>= (op stmt)) (>=        (M_value (operand1 stmt) state) (M_value (operand2 stmt) state)))
      ((eq? '&& (op stmt)) (and       (M_value (operand1 stmt) state) (M_value (operand2 stmt) state)))
      ((eq? '|| (op stmt)) (or        (M_value (operand1 stmt) state) (M_value (operand2 stmt) state)))
      ((eq? '!  (op stmt)) (not       (M_value (operand1 stmt) state)))
      (else  (error (cons "Symbol not recognized" (op stmt))))
      )))

;;; interpret_while ;;;
;;; interprets a while loop.
;; @stmt the while statement
;; @state the state of the current program
;; @return the return function
(define interpret_while (lambda (stmt state return)
    (call/cc (lambda (break)
      (letrec ((loop (lambda (condition body state)
          (if (M_value condition state)
            (loop condition body (call/cc (lambda (continue) (interpret_statement body state return break continue))))
            state))))
          (loop (operand1 stmt) (operand2 stmt) state)))
        )))



;;; state and stack-frames ;;;
;;; abstractions
(define newstate    (lambda ()        (cons (newstack) '())))
(define newstack  (lambda ()        '(()())))
(define names     (lambda (stack)   (car stack)))
(define vals      (lambda (stack)   (car (cdr stack))))
(define topstack  (lambda (state)     (car state)))
(define lowstacks (lambda (state)     (cdr state)))
(define pushstack (lambda (state)     (cons (newstack) state)))
(define popstack  (lambda (state)     (cdr state)))
(define instack?  (lambda (name stack) (not (= -1 (getIndex name (names stack))))))
(define getval    (lambda (name stack) (itemat (getIndex name (names stack)) (vals stack))))


;;; itemAt ;;;
;;; gets the i-th item in list. (0-based list)
;; @index the index of the item
;; @list the list to find the item
(define itemat (lambda (index list)
    (if (= index 0)
      (car list)
      (itemat (- index 1) (cdr list))
      )))

;;; removeAt ;;;
;;; removes the item in list l at index i, starting from 0.
;; @index the index of the item
;; @list the list to remove the item of
(define removeAt (lambda (index list)
    (if (= index 0)
      (cdr list)
      (cons (car list) (removeAt (- index 1) (cdr list)))
      )))


;;; replaceAt ;;;
;;; replaces the item in list at index i with x, starting at 0.
;; @name the variable of the item to replace
;; @i the index of the item to replace
;; @list the list that contains the item
(define replaceAt (lambda (name index list)
    (if (= index 0)
      (cons name (cdr list))
      (cons (car list) (replaceAt name (- index 1) (cdr list)))
      )))

;;; getIndex
;;; gets the given item's index
;; @name the name of the variable
;; @list the list to search
(define getIndex (lambda (name l)
    (letrec
      ((getIndex-cps (lambda (name l k)
        (cond
          ((null? l) -1)
          ((eq? name (car l)) (k 0))
          (else (getIndex-cps name (cdr l) (lambda (v) (k (+ v 1)))))
          ))))
      (getIndex-cps name l (lambda (v) v))
    )))

;;; declare ;;;
;;; declares a new variable in the current state (stack)
;; @name the name of the variable
;; @state the state
(define declare (lambda (name state)
  (cons
      (cons
        (cons name          (names (topstack state)))
      (cons (cons (box 0) (vals  (topstack state))) '()))
      (lowstacks state)
      )))

;;; declared? ;;;
;;; checks whether the variable is declared.
;; @name the name of the variable to check if it is declared.
;; @state the state
(define declared? (lambda (name state)
    (cond
      ((null? state) #f)
      ((instack? name (topstack state)) #t)
      (else (declared? name (lowstacks state)))
      )))

;;; assign ;;;
;;; sets the value of the variable in the current state (stack)
;; @name the name of the variable
;; @value the value of the variable
(define assign (lambda (name value state)
    (begin
      (cond
        ((declared? name state) (set-box! (lookup-ref name state) value))
        (else (assign name value (declare name state)))
        )
      state
      )))

;;; lookup-ref ;;;
;;; returns the value of the given variable name.
;; @name the name of the variable to find the value of
;; @state the state
(define lookup-ref (lambda (name state)
    (if (instack? name (topstack state))
      (getval name (topstack state))
      (lookup-ref name (lowstacks state))
      )))

(define lookup (lambda (name state) (unbox (lookup-ref name state))))


;;; Helper functions (abstractions)
(define operand1 (lambda (expr) (if (null? (cdr   expr)) '() (cadr   expr))))
(define operand2 (lambda (expr) (if (null? (cddr  expr)) '() (caddr  expr))))
(define operand3 (lambda (expr) (if (null? (cdddr expr)) '() (cadddr expr))))
(define op       (lambda (expr) (if (null? expr)         '() (car    expr))))
(define keywordOf op)

;;; quicktest, commented out.
(parser "test/test11.txt")
(interpret (parser "test/test11.txt"))