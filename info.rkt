#lang info

(define collection 'multi)

(define name "alexis-pvector")
(define version "0.2.0")

(define implies '("pvector"))

(define deps
  '("alexis-collections"
    "base"
    "pvector"
    "rackunit-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))
