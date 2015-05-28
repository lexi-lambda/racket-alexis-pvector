#lang racket/base

(require
  rackunit
  alexis/collection
  alexis/pvector)

(test-case
 "Basic vector operations"
 (check-equal? (sequence->list (pvector 1 2 3 4)) '(1 2 3 4))
 (check-equal? (sequence->list (conj* (pvector) 1 2 3 4)) '(1 2 3 4))
 (check-equal? (sequence->list (rest (pvector 1 2 3 4))) '(2 3 4))
 (check-equal? (sequence->list (reverse (pvector 1 2 3 4))) '(4 3 2 1)))

(test-case
 "Vector equality"
 (check-equal? (pvector 1 2 3 4) (conj* (pvector) 1 2 3 4)))
