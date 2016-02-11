; EECS 345 Class Project 1
; Hun Jae Lee, Daniel Yakobian, & Justin Wang
(load "simpleParser.scm")

;ENVIRONMENT

(define newenv '())

(define env-lookup
  (lambda (var env)
    (cond
     ((null? env) '())
     ((eq? var (first-var env)) (first-val env))
     (else (env-lookup var (cdr env))))))

(define env-bind
  (lambda (var val env)
    (cond
     ((expression? val)
      (cons (cons var (cons (value val env) '())) env))
     (else
      (cons (cons var (cons val '())) env)))))

(define env-update
  (lambda (var val env)
    (cond
     ((null? (env-lookup var env))
      (error "Variable undeclared"))
     (else
      (env-bind var val env)))))

(define first-var
  (lambda (env)
    (car (car env))))

(define first-val
  (lambda (env)
    (car (cdr (car env)))))
;environment

;INTERPRETER
(define interpret
  (lambda (filename)
    (env-lookup 'return (interpret-stmt-list
		     (parser filename)
		     newenv))))

(define interpret-stmt-list
  (lambda (parsetree env)
    (cond
     ((null? parsetree) env)
     (else (interpret-stmt-list (cdr parsetree)
				(interpret-stmt (car parsetree)
						env))))))

(define interpret-stmt
  (lambda (stmt env)
    (cond
     ((eq? '= (car stmt))
      "assign")
     ((eq? 'var (car stmt))
      "var")
     ((eq? 'if (car stmt))
      "if")
     ((eq? 'while (car stmt))
      "while")
     ((eq? 'return (car stmt))
      "return"))))