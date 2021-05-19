#lang racket
(begin
  (define (ack m n)
    (if (zero? m)
      (add1 n)
      (if (zero? n)
        (ack (sub1 m) 1)
        (ack (sub1 m) (ack m (sub1 n))))))
  (ack 3 12))
