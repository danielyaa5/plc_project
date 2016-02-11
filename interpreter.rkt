; EECS 345 Class Project 1
; Hun Jae Lee, Daniel Yakobian, & Justin Wang
(load "simpleParser.scm")

;interprets some code from a file
(define interpret
  (lambda (filename)
    (parser filename)
  ))

(define process-parse-tree
  (lambda (tree state)
    (cond
      ((eq? (identifier tree) 'var))
      ((eq? (identifier tree) '=))
      ((eq? (identifier tree) 'if))
      ((eq? (identifier tree) 'return))
      ((eq? (identifier tree) 'while))
      (else (error "bad-identifier" (identifier tree))))))

   
;the empty state
(define init-state `((() ())))

;parse tree elements
(define identifier caar)