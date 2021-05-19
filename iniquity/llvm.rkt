#lang racket

(provide value->string seq)

(define (value->string v)
  (cond
    [(symbol? v) (string-append "%" (symbol->string v))]
    [(number? v) (number->string v)]
    [(string? v) v]))

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
(define-binop Or "or")
(define-binop Ashr "ashr")
(define-binop Shl "shl")

(provide Alloca Br Store Load Select Call <- Ret Label Getelementptr
         Bitcast Ptrtoint Inttoptr Define Fastcall)

(define (Alloca [type "i64"])
  (string-append "alloca " type ", align 8"))

(define (Br r . ls)
  (match ls
    ['() (string-append "br label " (value->string r))]
    [(list l1 l2)
     (string-append "br i1 " (value->string r)  ", "
                    "label " (value->string l1) ", "
                    "label " (value->string l2))]))

(define (Store val to [type "i64"])
  (string-append "store " type " "
                 (value->string val) ", " type "* " (value->string to)
                 ", align 8"))

(define (Load from [type "i64"])
  (string-append "load " type ", " type "* " (value->string from) ", align 8"))

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

(define (Fastcall f rettype . args)
  (string-append
    "call fastcc " rettype " @" (symbol->string f) "("
    (string-join
      (map (λ (x) (string-append "i64 " (value->string x)))
           args)
      ", ")
    ")"))

(define (Ret v)
  (string-append "ret i64 " (value->string v)))

(define (Getelementptr p offset)
  (string-append "getelementptr i64, "
                 "i64* " (value->string p) ", "
                 "i64 " (value->string offset)))

(define (Label l)
  (string-append (symbol->string l) ":"))

(define (Bitcast v from to)
  (string-append "bitcast " from " " (value->string v) " to " to))

(define (Ptrtoint p)
  (string-append "ptrtoint i64* " (value->string p) " to i64"))

(define (Inttoptr i)
  (string-append "inttoptr i64 " (value->string i) " to i64*"))

(define (Define f args r c)
  (seq
    (string-append "define fastcc i64 @" (symbol->string f) "("
      (string-join
        (map (λ (x) (string-append "i64 " (value->string x)))
             args)
        ", ")
      ") {")
    c
    (Ret r)
    "}"))

;; assignment
;; r: register
;; v: string
(define (r . <- . v)
  (string-append
    (value->string r) " = "
    (if (string? v)
      v
      (Add 0 v))))
