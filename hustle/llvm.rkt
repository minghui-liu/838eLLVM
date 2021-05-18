#lang racket

(provide value->string seq)

(define (value->string v)
  (cond
    [(symbol? v) (string-append "%" (symbol->string v))]
    [(number? v) (number->string v)]))

(define (seq . xs)
  (string-join
    (foldr (λ (x is)
            (if (list? x)
              (append x is)
              (if (non-empty-string? x)
                (cons x is)
                is)))
           '()
           xs)
    "\n"))

;; define binary operation
(define-syntax-rule
  (define-binop Name inst)
  (begin
    (provide Name)
    (define (Name v1 v2)
      (string-append inst " i64 "
                     (value->string v1) ", " (value->string v2)))))

(define-binop Add "add")
(define-binop Sub "sub")
(define-binop Eq "icmp eq")
(define-binop Gt "icmp sgt")
(define-binop Lt "icmp slt")
(define-binop And "and")
(define-binop Xor "xor")
(define-binop Ashr "ashr")
(define-binop Shl "shl")

(provide Alloca Br Store Load Select Call <- Ret Label)

(define (Alloca)
  "alloca i64, align 8")

(define (Br r . ls)
  (match ls
    ['() (string-append "br label " (value->string r))]
    [(list l1 l2)
     (string-append "br i1 " (value->string r)  ", "
                    "label " (value->string l1) ", "
                    "label " (value->string l2))]))

(define (Store val to)
  (string-append "store i64 "
                 (value->string val) ", i64* " (value->string to)
                 ", align 8"))

(define (Load from)
  (string-append "load i64, i64* " (value->string from) ", align 8"))

(define (Select cnd v1 v2)
  (string-append
    "select i1" (value->string cnd) ", "
    "i64 " (value->string v1) ", "
    "i64 " (value->string v2)))

(define (Call f rettype . args)
  (string-append
    "call " rettype " @" (symbol->string f) "("
    (string-join
      (map (λ (x) (string-append "i64 " (value->string x)))
           args)
      ", ")
    ")"))

(define (Ret v)
  (string-append "ret i64 " (value->string v)))

(define (Label l)
  (string-append (symbol->string l) ":"))

;; assignment
;; r: register
;; v: string
(define (r . <- . v)
  (string-append
    (value->string r) " = "
    (if (string? v)
      v
      (Add 0 v))))
