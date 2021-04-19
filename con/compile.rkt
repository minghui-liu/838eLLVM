#lang racket
(provide (all-defined-out))
(require "ast.rkt")

;; Expr -> Asm
(define (compile e)
  (let ((r1 (gensym)))
  (string-append "define i64 @entry() {\n"
        (compile-e e r1)
        "ret i64 %" (symbol->string r1) "\n"
        "}\n")))

;; Expr -> Asm
(define (compile-e e r)
  (match e
    [(Int i)     (compile-integer i r)]
    [(Prim1 p e) (compile-prim1 p e r)]
    [(IfZero e1 e2 e3) (compile-ifzero e1 e2 e3 r)]))

;; Integer -> Asm
(define (compile-integer i r)
  (string-append "%" (symbol->string r) " = add i64 0, " (number->string i) "\n"))

;; Op Expr -> Asm
(define (compile-prim1 p e r)
  (let ((r1 (gensym)))
  (string-append (compile-e e r1)
       (match p
         ['add1 (string-append "%" (symbol->string r) " = add i64 %" (symbol->string r1) ", 1\n")]
         ['sub1 (string-append "%" (symbol->string r) " = sub i64 %" (symbol->string r1) ", 1\n")]))))

;; Expr Expr Expr -> Asm
(define (compile-ifzero e1 e2 e3 r)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if))
        (retval (gensym 'retval))
        (result (gensym 'result))
        (end (gensym 'end))
        (r1 (gensym))
        (r2 (gensym))
        (r3 (gensym)))
    (string-append
        "%" (symbol->string retval) " = alloca i64, align 4\n"
        (compile-e e1 r1)
        "%" (symbol->string result) " = icmp eq i64 0, %" (symbol->string r1) "\n"
        "br i1 %" (symbol->string result) ", label %" (symbol->string l1) ", label %" (symbol->string l2) "\n"
        (symbol->string l1) ":\n"
          (compile-e e2 r2)
          "store i64 %" (symbol->string r2) ", i64* %" (symbol->string retval) ", align 4\n"
          "br label %" (symbol->string end) "\n"
        (symbol->string l2) ":\n"
          (compile-e e3 r3)
          "store i64 %" (symbol->string r3) ", i64* %" (symbol->string retval) ", align 4\n"
          "br label %" (symbol->string end) "\n"
        (symbol->string end) ":\n"
          "%" (symbol->string r) " = load i64, i64* %" (symbol->string retval) "\n"
        ))) 
