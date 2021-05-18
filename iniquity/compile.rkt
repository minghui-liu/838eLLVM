#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "llvm.rkt")

;; Prog -> LLVM IR
(define (compile p)
  (match p
    [(Prog ds e)
     (let-values ([(r c) (compile-e e '())])
       (seq
         "declare i64 @peek_byte()"
         "declare i64 @read_byte()"
         "declare i64 @write_byte(i64)"
         "declare void @raise_error()"
         "define i64 @entry(i64* %heap) {"
         ('top . <- . (Alloca "i64*"))
         (Store 'heap 'top "i64*")
         c
         (Ret r)
         "}"
         (compile-defines ds)))]))

;; Expr CEnv -> (Value, LLVM IR)
;; Each expression will be compiled into
;;   1. a Value, which can be either a register
;;      label or an immediate
;;   2. LLVM IR as a string that assigns the
;;      result to Value
;; Value will be available after the assembly is
;; executed. The assembly can be empty if Expr
;; produces an immediate.
;;
;; For user:
;;   (let-values ([(r c) (compile-e e)])
;;     (seq
;;       c         ; paste compiled IR
;;       (Ret r))) ; value now stored in r
;;
;; For compiler:
;;   (define (compile-e e)
;;     (ret-reg r
;;       instructions))
;;
;; We are using Racket's multiple return value here.
(define (compile-e e env)
  (match e
    [(Int i)          (compile-imm i)]
    [(Bool b)         (compile-imm b)]
    [(Char c)         (compile-imm c)]
    [(Eof)            (compile-imm eof)]
    [(Empty)          (compile-imm '())]
    [(Var x)          (compile-variable x env)]
    [(Prim0 p)        (compile-prim0 p env)]
    [(Prim1 p e)      (compile-prim1 p e env)]
    [(Prim2 p e1 e2)  (compile-prim2 p e1 e2 env)]
    [(If e1 t f)      (compile-if e1 t f env)]
    [(Begin e1 e2)    (compile-begin e1 e2 env)]
    [(Let x e1 e2)    (compile-let x e1 e2 env)]
    [(App f es)       (compile-app f es env)]))

;; return immediate
(define-syntax-rule
  (ret-imm v)
  (values v (seq))) ; for immediates, no instructions are generated

;; return register
(define-syntax-rule
  (ret-reg r body)
  (let ([r (gensym 'ret)])
    (println r (current-error-port))
    (println body (current-error-port))
    (values r body)))

;; Immediate -> (Value, LLVM IR)
(define (compile-imm v)
  (ret-imm (imm->bits v)))

;; Id -> (Value, LLVM IR)
(define (compile-variable x env)
  (ret-imm (lookup x env)))

;; Op0 -> (Value, LLVM IR)
(define (compile-prim0 p env)
  (ret-reg r
    (match p
      ['void       (r . <- . val-void)]
      ['read-byte  (r . <- . (Call 'read_byte "i64"))]
      ['peek-byte  (r . <- . (Call 'peek_byte "i64"))])))

;; Op Expr -> LLVM IR
(define (compile-prim1 p e env)
  (ret-reg r
    (let-values ([(r1 c1) (compile-e e env)])
      (seq
        c1
        (match p
          ['add1
           (seq
             (assert-integer r1)
             (r . <- . (Add r1 (imm->bits 1))))]
          ['sub1
           (seq
             (assert-integer r1)
             (r . <- . (Sub r1 (imm->bits 1))))]
          ['zero?
           (let ([rcmp (gensym 'cmp)])
             (seq
               (assert-integer r1)
               (rcmp . <- . (Eq r1 0))
               (r . <- . (Select rcmp val-true val-false))))]
          ['char?
           (let ([v1 (gensym)]
                 [v2 (gensym)]
                 [rcmp (gensym 'cmp)])
             (seq
               (v1 . <- . (And r1 mask-char))
               (v2 . <- . (Xor v1 type-char))
               (rcmp . <- . (Eq v2 0))
               (r  . <- . (Select rcmp val-true val-false))))]
          ['char->integer
           (let ([v1 (gensym)])
             (seq
               (assert-char r1)
               (v1 . <- . (Ashr r1 char-shift))
               (r  . <- . (Shl v1 int-shift))))]
          ['integer->char
           (let ([v1 (gensym)]
                 [v2 (gensym)])
             (seq
               (assert-codepoint r1)
               (v1 . <- . (Ashr r1 int-shift))
               (v2 . <- . (Shl  v1 char-shift))
               (r  . <- . (Xor  v2 type-char))))]
          ['eof-object?
           (let ([rcmp (gensym 'cmp)])
             (seq
               (rcmp . <- . (Eq r1 val-eof))
               (r . <- . (Select rcmp val-true val-false))))]
          ['empty?
           (let ([rcmp (gensym 'cmp)])
             (seq
               (rcmp . <- . (Eq r1 val-empty))
               (r . <- . (Select rcmp val-true val-false))))]
          ['write-byte
           (seq
             (assert-byte r1)
             (Call 'write_byte "i64" r1)
             (r . <- . val-void ))]
          ['unbox
           (let ([v1 (gensym)]
                 [addr (gensym)])
             (seq
               (assert-box r1)
               (v1 . <- . (Xor r1 type-box))
               (addr . <- . (Inttoptr v1))
               (r  . <- . (Load addr))))]
          ['box
           (let ([addr (gensym)]
                 [p1 (gensym)]
                 [p2 (gensym)]
                 [newtop (gensym)])
             (seq
               (addr . <- . (Load 'top "i64*"))
               (Store r1 addr)
               ; increment heap top ptr
               (newtop . <- . (Getelementptr addr 1))
               (Store newtop 'top "i64*")
               ; pointer tagging
               (p1 . <- . (Getelementptr addr 0))
               (p2 . <- . (Ptrtoint p1))
               (r  . <- . (Or p2 type-box))))]
          ['car
           (let ([v1 (gensym)]
                 [addr1 (gensym)]
                 [addr2 (gensym)])
             (seq
               (assert-cons r1)
               (v1 . <- . (Xor r1 type-cons))
               ; r = *(p+1)
               (addr1 . <- . (Inttoptr v1))
               (addr2 . <- . (Getelementptr addr1 1))
               (r  . <- . (Load addr2))))]
          ['cdr
           (let ([v1 (gensym)]
                 [addr (gensym)])
             (seq
               (assert-cons r1)
               (v1 . <- . (Xor r1 type-cons))
               ; r = *p
               (addr . <- . (Inttoptr v1))
               (r  . <- . (Load addr))))])))))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 env)
  (ret-reg r
    (let-values ([(r1 c1) (compile-e e1 env)]
                 [(r2 c2) (compile-e e2 env)])
      (seq
        c1
        c2
        (match p
          ['+
           (seq
             (assert-integer r1)
             (assert-integer r2)
             (r . <- . (Add r1 r2)))]
          ['-
           (seq
             (assert-integer r1)
             (assert-integer r2)
             (r . <- . (Sub r1 r2)))]
          ['eq?
           (let ([rcmp (gensym 'cmp)])
             (seq
               (rcmp . <- . (Eq r1 r2))
               (r . <- . (Select rcmp val-true val-false))))]
          ['cons
           (let ([addr1 (gensym)]
                 [addr2 (gensym)]
                 [p1 (gensym)]
                 [p2 (gensym)]
                 [newtop (gensym)])
             (seq
               (addr1 . <- . (Load 'top "i64*"))      ; cdr
               (addr2 . <- . (Getelementptr addr1 1)) ; car
               (Store r1 addr2)
               (Store r2 addr1)
               ; increment heap top ptr
               (newtop . <- . (Getelementptr addr1 2))
               (Store newtop 'top "i64*")
               ; pointer tagging
               (p1 . <- . (Getelementptr addr1 0))
               (p2 . <- . (Ptrtoint p1))
               (r  . <- . (Or p2 type-cons))))])))))

;; Expr Expr Expr -> LLVM IR
(define (compile-if cnd t f env)
  (ret-reg r
    (let-values ([(rcond ccond) (compile-e cnd env)]
                 [(rt ct) (compile-e t env)]
                 [(rf cf) (compile-e f env)]
                 [(ltrue) (gensym 'if)]
                 [(lfalse) (gensym 'if)]
                 [(lend) (gensym 'if)]
                 [(ret) (gensym 'ifret)]
                 [(rcmp) (gensym 'cmp)])
      (seq
        (ret . <- . (Alloca))

        ccond
        (rcmp . <- . (Eq rcond val-false))
        (Br rcmp lfalse ltrue)

        (Label ltrue)
          ct
          (Store rt ret)
          (Br lend)

        (Label lfalse)
          cf
          (Store rf ret)
          (Br lend)

        (Label lend)
          (r . <- . (Load ret))))))

;; Expr Expr -> LLVM IR
(define (compile-begin e1 e2 env)
  (ret-reg r
    (let-values ([(r1 c1) (compile-e e1 env)]
                 [(r2 c2) (compile-e e2 env)])
      (seq
        c1
        c2
        (r . <- . r2)))))

;; Id Expr Expr CEnv -> (Value, LLVM IR)
(define (compile-let x e1 e2 env)
  (ret-reg r
    (let*-values ([(r0)     (gensym 'var)]
                  [(r1 c1) (compile-e e1 env)]
                  [(r2 c2) (compile-e e2 (cons `(,x . ,r0) env))])
      (seq
        ; binding -> r0
        c1
        (r0 . <- . r1)
        ; body -> return
        c2
        (r . <- . r2)))))

(define (compile-app f es env)
  (ret-reg r
    (let* ([rcs (compile-es es env)]
           [rs (map car rcs)]
           [cs (map cdr rcs)]
           [xs (map (λ (x)
                       (if (symbol? x)
                         (lookup x env)
                         x)) rs)])
      (seq
        cs
        (r . <- . (apply (curry Call (symbol->label f) "i64") xs))))))

(define (compile-es es env)
  (match es
    ['() '()]
    [(cons e es)
     (let-values ([(r c) (compile-e e env)])
       (cons `(,r . ,c) (compile-es es env)))]))

;; [Listof Defn] -> LLVM IR
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))

;; Defn -> LLVM IR
(define (compile-define d)
  (match d
    [(Defn f xs e)
     (let*-values ([(env) (genenv xs)]
                   [(r c) (compile-e e env)])
       (Define (symbol->label f) (map cdr env) r c))]))

(define (genenv xs)
  (match xs
    ['() '()]
    [(cons x xs) (cons `(,x . ,(gensym 'var)) (genenv xs))]))

(define (assert-type mask type)
  (λ (arg)
     (let ([r1   (gensym)]
           [rcmp (gensym 'cmp)]
           [lerr (gensym 'assert)]
           [lend (gensym 'assert)])
       (seq
         (r1 . <- . (And arg mask))
         (rcmp . <- . (Eq r1 type))
         (Br rcmp lend lerr)

         (Label lerr)
          (Call 'raise_error "void")
          (Br lend)

         (Label lend)))))

(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))
(define assert-box
  (assert-type ptr-mask type-box))
(define assert-cons
  (assert-type ptr-mask type-cons))

(define (assert-byte arg)
 (let ([rcmp1 (gensym 'cmp)]
       [rcmp2 (gensym 'cmp)]
       [lerr  (gensym 'assert)]
       [l1    (gensym 'assert)]
       [lend  (gensym 'assert)])
   (seq
     (assert-integer arg)
     (rcmp1 . <- . (Lt arg (imm->bits 0)))
     (Br rcmp1 lerr l1)

     (Label l1)
       (rcmp2 . <- . (Gt arg (imm->bits 255)))
       (Br rcmp2 lerr lend)

     (Label lerr)
       (Call 'raise_error "void")
       (Br lend)

     (Label lend))))

(define (assert-codepoint arg)
 (let ([rcmp1 (gensym 'cmp)]
       [rcmp2 (gensym 'cmp)]
       [rcmp3 (gensym 'cmp)]
       [rcmp4 (gensym 'cmp)]
       [lerr  (gensym 'assert)]
       [l1    (gensym 'assert)]
       [l2    (gensym 'assert)]
       [l3    (gensym 'assert)]
       [lend  (gensym 'assert)])
   (seq
     (assert-integer arg)
     (rcmp1 . <- . (Lt arg (imm->bits 0)))
     (Br rcmp1 lerr l1)      ; x < 0 -> err

     (Label l1)
       (rcmp2 . <- . (Gt arg (imm->bits 1114111)))
       (Br rcmp2 lerr l2)    ; x > 1114111 -> err

     (Label l2)
       (rcmp3 . <- . (Lt arg (imm->bits 55295)))
       (Br rcmp3 lend l3)    ; x < 55295 -> ok

     (Label l3)
       (rcmp4 . <- . (Gt arg (imm->bits 57344)))
       (Br rcmp4 lend lerr)  ; x > 57344 -> ok

     (Label lerr)
       (Call 'raise_error "void")
       (Br lend)

     (Label lend))))

;; cenv is a mapping Symbol -> Symbol
;; we need this because SSA doesn't allow
;; varaible name shadowing
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons (cons v r) rest)
     (match (eq? x v)
       [#t r]
       [#f (lookup x rest)])]))

;; Symbol -> Label
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))
