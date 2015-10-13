#lang racket/base

(provide build-pvector make-pvector)

(require alexis/pvector/base
         alexis/pvector/for)

(define (build-pvector n f)
  (for/pvector ([i (in-range n)]) (f i)))

(define (make-pvector n v)
  (for/pvector ([i (in-range n)]) v))

