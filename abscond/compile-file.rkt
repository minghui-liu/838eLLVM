#lang racket
(provide main)
(require "parse.rkt" "compile.rkt")

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (let ((p (open-input-file fn)))
    (begin
      (read-line p) ; ignore #lang racket line
      (displayln (compile (parse (read p))))
      (close-input-port p))))
