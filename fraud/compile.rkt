#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt")

;; Expr ->  LLVM IR
(define (compile e)
  (let ((r1 (gensym)))
    (string-append 
      "declare i64 @peek_byte()\n"
      "declare i64 @read_byte()\n"
      "declare i64 @write_byte(i64)\n"
      "define i64 @entry() {\n"
      (compile-e e r1)
      "ret i64 %" (symbol->string r1) "\n"
      "}\n")))

;; Expr ->  LLVM IR
(define (compile-e e r)
  (match e
    [(Int i)          (compile-integer i r)]
    [(Bool b)         (compile-value b r)] 
    [(Char c)         (compile-value c r)]
    [(Eof)            (compile-value eof)]
    [(Var x)          (compile-variable x r)]
    [(Prim0 p)        (compile-prim0 p r)]
    [(Prim1 p e)      (compile-prim1 p e r)]
    [(Prim2 p e1 e2)  (compile-prim2 p e1 e2  r)]
    [(If e1 e2 e3)    (compile-if e1 e2 e3 r)]
    [(Begin e1 e2)    (compile-begin e1 e2 r)]
    [(Let x e1 e2)    (compile-let x e1 e2 r)]))

;; Integer -> LLVM IR
(define (compile-integer i r)
  (string-append "%" (symbol->string r) " = add i64 0, " (number->string (value->bits i)) "\n"))

;; Value -> LLVM IR
(define (compile-value v r)
  (string-append "%" (symbol->string r) " = add i64 0, " (number->string (value->bits v)) "\n"))

;; Id CEnv -> LLVM IR
(define (compile-variable x r)
  (string-append "%" (symbol->string r) " = load i64, i64* %" (symbol->string x) "\n"))

;; Op0 -> LLVM IR
(define (compile-prim0 p r)
  (match p
    ['void
      (string-append "%" (symbol->string r) " = add i64 0, " (number->string val-void) "\n")]
    ['read-byte
      (string-append "%" (symbol->string r) " = call i64 @read_byte()\n")]
    ['peek-byte
      (string-append "%" (symbol->string r) " = call i64 @peek_byte()\n")]))

;; Op Expr -> LLVM IR
(define (compile-prim1 p e r)
  (let ((r1 (gensym)))
  (string-append (compile-e e r1)
    (match p
      ['add1 (string-append "%" (symbol->string r) " = add i64 %" (symbol->string r1) ", " (number->string (value->bits 1)) "\n")]
      ['sub1 (string-append "%" (symbol->string r) " = sub i64 %" (symbol->string r1) ", " (number->string (value->bits 1)) "\n")]
      ['zero?
        (let ((result (gensym 'result)))
          (string-append 
            "%" (symbol->string result) " = icmp eq i64 0, %" (symbol->string r1) "\n"
            "%" (symbol->string r) " = select i1 %" (symbol->string result) ", i64 " (number->string val-true) ", i64 " (number->string val-false) "\n"))]
      ['char?
        (let ((v1 (gensym))
              (v2 (gensym))
              (result (gensym 'result)))
          (string-append
            "%" (symbol->string v1) " = and i64 %" (symbol->string r1) ", " (number->string mask-char) "\n"
            "%" (symbol->string v2) " = xor i64 %" (symbol->string v1) ", " (number->string type-char) "\n"
            "%" (symbol->string result) " = icmp eq i64 0, %" (symbol->string v2) "\n"
            "%" (symbol->string r) " = select i1 %" (symbol->string result) ", i64 " (number->string val-true) ", i64 " (number->string val-false) "\n"))]
      ['char->integer
        (let ((v1 (gensym)))
          (string-append
            "%" (symbol->string v1) " = ashr i64 %" (symbol->string r1) ", " (number->string char-shift) "\n"
            "%" (symbol->string r) " = shl i64 %" (symbol->string v1) ", " (number->string int-shift) "\n"
          )
        )]
      ['integer->char
        (let ((v1 (gensym))
              (v2 (gensym)))
          (string-append
            "%" (symbol->string v1) " = ashr i64 %" (symbol->string r1) ", " (number->string int-shift) "\n"
            "%" (symbol->string v2) " = shl i64 %" (symbol->string v1) ", " (number->string char-shift) "\n"
            "%" (symbol->string r) " = xor i64 %" (symbol->string v2) ", " (number->string type-char) "\n"))]
      ['eof-object?
        (let ((result (gensym 'result)))
          (string-append
            "%" (symbol->string result) " = icmp eq i64 " (number->string val-eof) ", %" (symbol->string r1) "\n"
            "%" (symbol->string r) " = select i1 %" (symbol->string result) ", i64 " (number->string val-true) ", i64 " (number->string val-false) "\n")
          )]
      ['write-byte
        (let ()
          (string-append
            "call i64 @write_byte(i64 %" (symbol->string r1) ")\n"
            "%" (symbol->string r) " = add i64 0, " (number->string val-void) "\n"))]
      ))))
  
;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 r)
  (let ((r1 (gensym))
        (r2 (gensym)))
    (string-append 
      (compile-e e1 r1)
      (compile-e e2 r2)
      (match p
        ['+
          (string-append "%" (symbol->string r) " = add i64 %" (symbol->string r1) ", %" (symbol->string r2) "\n")]
        ['-
          (string-append "%" (symbol->string r) " = sub i64 %" (symbol->string r1) ", %" (symbol->string r2) "\n")]
      ))))

;; Expr Expr Expr -> LLVM IR
(define (compile-if e1 e2 e3 r)
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
        "%" (symbol->string result) " = icmp eq i64 " (number->string val-true)  ", %" (symbol->string r1) "\n"
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

;; Expr Expr -> LLVM IR
(define (compile-begin e1 e2 r)
  (let ((r1 (gensym)))
    (string-append
      (compile-e e1 r1)
      (compile-e e2 r)
    )))

;; Id Expr Expr CEnv -> LLVM IR
(define (compile-let x e1 e2 r)
  (let ((r1 (gensym)))
    (string-append
      "%" (symbol->string x) " = alloca i64\n"
      (compile-e e1 r1)
      "store i64 %" (symbol->string r1) ", i64* %" (symbol->string x) "\n"
      (compile-e e2 r))))