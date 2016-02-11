; EECS 345 Class Project 1
; Hun Jae Lee, Daniel Yakobian, & Justin Wang
(load "simpleParser.scm")

;interprets some code from a file
(define interpret
  (lambda (filename)
  	; Feed file into parser
  	; Evaluate the parse tree returned by parser
  	; Return the appropriate value
    (parser filename)
  ))