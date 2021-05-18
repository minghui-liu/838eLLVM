#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "llvm.rkt")

;; Expr -> LLVM IR
(define (compile e)
  (let-values ([(r c) (compile-e e)])
    (seq
      "declare i64 @peek_byte()"
      "declare i64 @read_byte()"
      "declare i64 @write_byte(i64)"
      "define i64 @entry() {"
      c
      (Ret r)
      "}")))

;; Expr -> (Value, LLVM IR)
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
(define (compile-e e)
  (match e
    [(Int i)          (compile-imm i)]
    [(Bool b)         (compile-imm b)]
    [(Char c)         (compile-imm c)]
    [(Eof)            (compile-imm eof)]
    [(Var x)          (compile-variable x)]
    [(Prim0 p)        (compile-prim0 p)]
    [(Prim1 p e)      (compile-prim1 p e)]
    [(Prim2 p e1 e2)  (compile-prim2 p e1 e2)]
    [(If e1 t f)      (compile-if e1 t f)]
    [(Begin e1 e2)    (compile-begin e1 e2)]
    [(Let x e1 e2)    (compile-let x e1 e2)]))

;; return immediate
(define-syntax-rule
  (ret-imm v)
  (values v (seq))) ; for immediates, no instructions are generated

;; return register
(define-syntax-rule
  (ret-reg r body)
  (let ([r (gensym 'ret)])
    (values r body)))

;; Immediate -> (Value, LLVM IR)
(define (compile-imm v)
  (ret-imm (imm->bits v)))

;; Id -> (Value, LLVM IR)
(define (compile-variable x)
  (ret-reg r
    (r . <- . (Load x))))

;; Op0 -> (Value, LLVM IR)
(define (compile-prim0 p)
  (ret-reg r
    (match p
      ['void       (r . <- . val-void)]
      ['read-byte  (r . <- . (Call 'read_byte "i64"))]
      ['peek-byte  (r . <- . (Call 'peek_byte "i64"))])))

;; Op Expr -> LLVM IR
(define (compile-prim1 p e)
  (ret-reg r
    (let-values ([(r1 c1) (compile-e e)])
      (seq
        c1
        (match p
          ['add1
           (r . <- . (Add r1 (imm->bits 1)))]
          ['sub1
           (r . <- . (Sub r1 (imm->bits 1)))]
          ['zero?
           (let ([rcmp (gensym 'cmp)])
             (seq
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
               (v1 . <- . (Ashr r1 char-shift))
               (r  . <- . (Shl v1 int-shift))))]
          ['integer->char
           (let ([v1 (gensym)]
                 [v2 (gensym)])
             (seq
               (v1 . <- . (Ashr r1 int-shift))
               (v2 . <- . (Shl  v1 char-shift))
               (r  . <- . (Xor  v2 type-char))))]
          ['eof-object?
           (let ([rcmp (gensym 'cmp)])
             (seq
               (rcmp . <- . (Eq r1 val-eof))
               (r . <- . (Select rcmp val-true val-false))))]
          ['write-byte
           (seq
             (Call 'write_byte "i64" r1)
             (r . <- . val-void))])))))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2)
  (ret-reg r
    (let-values ([(r1 c1) (compile-e e1)]
                 [(r2 c2) (compile-e e2)])
      (seq
        c1
        c2
        (match p
          ['+ (r . <- . (Add r1 r2))]
          ['- (r . <- . (Sub r1 r2))])))))

;; Expr Expr Expr -> LLVM IR
(define (compile-if cnd t f)
  (ret-reg r
    (let-values ([(rcnd ccnd) (compile-e cnd)]
                 [(rt ct) (compile-e t)]
                 [(rf cf) (compile-e f)]
                 [(ltrue) (gensym 'if)]
                 [(lfalse) (gensym 'if)]
                 [(lend) (gensym 'if)]
                 [(ret) (gensym 'ifret)]
                 [(rcmp) (gensym 'cmp)])
      (seq
        (ret . <- . (Alloca))

        ccnd
        (rcmp . <- . (Eq rcnd val-true))
        (Br rcmp ltrue lfalse)

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
(define (compile-begin e1 e2 r)
  (ret-reg r
    (let-values ([(r1 c1) (compile-e e1)]
                 [(r2 c2) (compile-e e2)])
      (seq
        c1
        c2
        (r . <- . r2)))))

;; Id Expr Expr CEnv -> (Value, LLVM IR)
(define (compile-let x e1 e2)
  (ret-reg r
    (let-values ([(r1 c1) (compile-e e1)]
                 [(r2 c2) (compile-e e2)])
      (seq
        (x . <- . (Alloca))
        ; binding -> x
        c1
        (Store r1 x)
        ; body -> return
        c2
        (r . <- . r2)))))
