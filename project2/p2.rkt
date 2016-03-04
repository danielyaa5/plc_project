(load "simpleParser.scm")


; Interprets a file and returns the result.
(define interpret (lambda (filename)
    (breakdown (call/cc (lambda (ret)
      (read_statements
        filename
        (newstate)
        ret
        (lambda (v) (error "Unacceptable break statement"))
        (lambda (v) (error "Unacceptable continue statement"))
        ))))))

; Returns the value to human-readable format.
(define breakdown (lambda (x)
    (cond
      ((and (not (number? x)) x)       "true" )
      ((and (not (number? x)) (not x)) "false")
      (else x)
      )))

; Interprets a list of parsed statements.
(define read_statements (lambda (statements state return break cont)
    (if (null? statements)
     state
     (read_statements (cdr statements) (interpret_statement (car statements) state return break cont) return break cont)
     )))

; Interprets a single statement.
(define interpret_statement (lambda (stmt state ret break cont)
    (cond
      ((eq? 'return   (keywordOf stmt)) (ret (M_value (operand1 stmt) state)     ))
      ((eq? '=        (keywordOf stmt)) (M_assign          stmt state               ))
      ((eq? 'var      (keywordOf stmt)) (M_declare         stmt state               ))
      ((eq? 'if       (keywordOf stmt)) (interpret_if      stmt state ret break cont))
      ((eq? 'while    (keywordOf stmt)) (interpret_while   stmt state ret           ))
      ((eq? 'begin    (keywordOf stmt)) (interpret_begin   stmt state ret break cont))
      ((eq? 'break    (keywordOf stmt)) (break                  state               ))
      ((eq? 'continue (keywordOf stmt)) (cont                   state               ))
      (else                             (M_value   stmt state               ))
      )))



; Interprets an assignment (e.g. "x = 10;").
(define M_assign (lambda (stmt state)
    (assign (operand1 stmt) (M_value (operand2 stmt) state) state)
    ))

; Interprets a block (e.g. "{...}").
(define interpret_begin (lambda (stmt state ret break cont)
    (popframe
      (read_statements
        (cdr stmt)
        (pushframe state)
        ret
        (lambda (v)
          (break (popframe v)))
        (lambda (v)
          (cont (popframe v)))
        ))))

; Interprets a declaration (e.g. "var x;" or "var y = 10").
(define M_declare (lambda (stmt state)
    (if (null? (operand2 stmt))
      (declare (operand1 stmt) state)
      (assign (operand1 stmt) (M_value (operand2 stmt) state) (declare (operand1 stmt) state))
      )))

; Interprets an if statement (e.g. "if (...) ...;" or "if (...) {...} else {...}").
(define interpret_if (lambda (stmt state ret break cont)
    (cond
      ((M_value (operand1 stmt) state) (interpret_statement (operand2 stmt) state ret break cont))
      ((null? (operand3 stmt)) state)
      (else (interpret_statement (operand3 stmt) state ret break cont))
      )))

; Interprets the value of a mathematical statement.
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

; Interprets the value of a while loop.
(define interpret_while (lambda (stmt state ret)
    (call/cc (lambda (break)
      (letrec ((loop (lambda (test body state)
          (if (M_value test state)
            (loop test body (call/cc (lambda (cont) (interpret_statement body state ret break cont))))
            state))))
          (loop (operand1 stmt) (operand2 stmt) state)))
        )))

; The stateironment is stored as a list of frames, ordered by most recent frame
; first. Each frame has two lists of equal size, the first of variable names,
; and the second of their values.

; Generates a new stateironment. 
(define newstate    (lambda ()        (cons (newframe) '())))

; Frame abstractions.
(define newframe  (lambda ()        '(()())))
(define names     (lambda (frame)   (car frame)))
(define vals      (lambda (frame)   (car (cdr frame))))
(define topframe  (lambda (state)     (car state)))
(define lowframes (lambda (state)     (cdr state)))
(define pushframe (lambda (state)     (cons (newframe) state)))
(define popframe  (lambda (state)     (cdr state)))
(define inframe?  (lambda (name frame) (not (= -1 (getIndex name (names frame))))))
(define getval    (lambda (name frame) (itemat (getIndex name (names frame)) (vals frame))))

; Gets the item in list l at index i, starting from 0.
(define itemat (lambda (i l)
    (if (= i 0)
      (car l)
      (itemat (- i 1) (cdr l))
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
        (cons name          (names (topframe state)))
      (cons (cons (box 0) (vals  (topframe state))) '()))
      (lowframes state)
      )))

;;; declared? ;;;
;;; checks whether the variable is declared.
;; @name the name of the variable to check if it is declared.
;; @state the state
(define declared? (lambda (name state)
    (cond
      ((null? state) #f)
      ((inframe? name (topframe state)) #t)
      (else (declared? name (lowframes state)))
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
    (if (inframe? name (topframe state))
      (getval name (topframe state))
      (lookup-ref name (lowframes state))
      )))

(define lookup (lambda (name state) (unbox (lookup-ref name state))))


;;; Helper functions (abstractions)
(define operand1 (lambda (expr) (if (null? (cdr   expr)) '() (cadr   expr))))
(define operand2 (lambda (expr) (if (null? (cddr  expr)) '() (caddr  expr))))
(define operand3 (lambda (expr) (if (null? (cdddr expr)) '() (cadddr expr))))
(define op       (lambda (expr) (if (null? expr)         '() (car    expr))))
(define keywordOf op)

;;; quicktest, commented out.
(parser "test/test1.txt")
(interpret (parser "test/test1.txt"))