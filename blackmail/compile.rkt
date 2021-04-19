#lang racket
(provide (all-defined-out))
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
    [(Prim1 p e) (compile-prim1 p e r)]
    [(Int i)     (compile-integer i r)]))

;; Op Expr -> Asm
(define (compile-prim1 p e r)
  (let ((l1 (gensym)))
  (string-append (compile-e e l1)
       (match p
         ['add1 (string-append "%" (symbol->string r) " = add i64 %" (symbol->string l1) ", 1;\n")]
         ['sub1 (string-append "%" (symbol->string r) " = sub i64 %" (symbol->string l1) ", 1;\n")]))))

;; Integer -> Asm
(define (compile-integer i r)
  (string-append "%" (symbol->string r) " = add i64 0, " (number->string i) ";\n"))
