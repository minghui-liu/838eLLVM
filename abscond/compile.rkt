#lang racket
(provide compile)
(require "ast.rkt")

;; Expr -> Asm
(define (compile e)
  (let ((l1 (gensym)))
  (string-append "define i64 @entry() {\n"
        (compile-e e l1)
        "ret i64 %" (symbol->string l1) ";\n"
        "}\n")))

;; Expr -> Asm
(define (compile-e e r)
  (match e
    [(Int i)     (compile-integer i r)]))

;; Integer -> Asm
(define (compile-integer i r)
  (string-append "%" (symbol->string r) " = add i64 0, " (number->string i) ";\n"))
