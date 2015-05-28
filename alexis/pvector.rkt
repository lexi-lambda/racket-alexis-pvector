#lang racket/base

(require
  racket/match
  racket/vector
  alexis/collection
  racket/generic
  racket/contract)

(provide pvector pvector?)

;; ---------------------------------------------------------------------------------------------------

(define bits-per-node 5)
(define branching-factor (expt 2 bits-per-node))

(struct pvector-head (size root tail)
  #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b equal?)
     (and (= (length a) (length b))
          (andmap equal? a b)))
   (define (hash-proc a hash-code)
     (foldl (λ (acc v) (bitwise-xor acc (hash-code v))) 0 a))
   (define (hash2-proc a hash-code)
     (foldl (λ (acc v) (bitwise-xor acc (hash-code v))) 0 a))]
  #:methods gen:countable
  [(define (length pv) (pvector-length pv))
   (define (known-finite? pv) #t)]
  #:methods gen:collection
  [(define (conj pv e) (pvector-conj pv e))]
  #:methods gen:sequence
  [(define/generic -rest rest)
   (define/generic -reverse reverse)
   (define (nth pv i) (pvector-get pv i))
   (define (set-nth pv i v) (pvector-set pv i v))
   (define (random-access? pv) #t)])
(struct pvector-tail (size children) #:transparent)
(struct pvector-node (height children) #:transparent)

;; ---------------------------------------------------------------------------------------------------

(define (pvector? v)
  (pvector-head? v))

(define (pvector . args)
  (extend pvector-empty args))

(define (pvector-empty? head)
  (zero? (pvector-head-size head)))

(define (pvector-length head)
  (pvector-head-size head))

(define (pvector-get head index)
  (match-define (pvector-head size root tail) head)
  (when (>= index size)
    (raise-range-error 'pvector-get "vector" "" index head 0 (sub1 size)))
  (define tail-offset (- size (pvector-tail-size tail)))
  (if (>= index tail-offset)
      ; when the element is in the tail, grab it directly
      (vector-ref (pvector-tail-children tail) (- index tail-offset))
      ; otherwise perform tree lookup
      (pvector-node-get root index)))

(define (pvector-set head index value)
  (match-define (pvector-head size root tail) head)
  (when (>= index size)
    (raise-range-error 'pvector-set "vector" "" index head 0 (sub1 size)))
  (define tail-offset (- size (pvector-tail-size tail)))
  (if (>= index tail-offset)
      ; when the element is in the tail, update it directly
      (pvector-head size root (pvector-tail-set tail (- index tail-offset) value))
      ; otherwise perform tree modification
      (pvector-head size (pvector-node-set root index value) tail)))

(define (pvector-conj head value)
  (match-define (pvector-head size root tail) head)
  (define tail-offset (- size (pvector-tail-size tail)))
  (cond
    ; if there's room in the tail, just stick it there
    [(< (pvector-tail-size tail) branching-factor)
     (pvector-head (add1 size) root (pvector-tail-conj tail value))]
    ; otherwise, we need to move the tail into the tree and create a new tail
    [else
     (define new-tail (make-pvector-tail 1))
     (vector-set! (pvector-tail-children new-tail) 0 value)
     (cond
       ; in the case of the empty tree, the old tail becomes the tree
       [(not root)
        (pvector-head (add1 size) (pvector-node 0 (pvector-tail-children tail)) new-tail)]
       ; handle the case where the tree isn't big enough, so we need to make a new root
       [(> (add1 tail-offset) (pvector-node-capacity root))
        (define new-root (make-pvector-node (add1 (pvector-node-height root))))
        (vector-set! (pvector-node-children new-root) 0 root)
        (pvector-head (add1 size)
                      (pvector-node-conj new-root tail-offset (pvector-tail->leaf tail))
                      new-tail)]
       ; otherwise just set the right index
       [else
        (pvector-head (add1 size)
                      (pvector-node-conj root tail-offset (pvector-tail->leaf tail))
                      new-tail)])]))

(define (pvector-pop head)
  (match-define (pvector-head size root tail) head)
  (define tail-offset (- size (pvector-tail-size tail)))
  (cond
    ; if there's only one element left, just return the empty vector
    [(= 1 size) pvector-empty]
    ; if there's more than one element in the tail, just remove it from there
    [(> (pvector-tail-size tail) 1)
     (pvector-head (sub1 size) root (pvector-tail-pop tail))]
    ; if root is a leaf, just use it as the tail
    [(zero? (pvector-node-height root))
     (pvector-head (sub1 size) #f (pvector-tail (sub1 size) (pvector-node-children root)))]
    ; otherwise, we need to lift a new tail out of the tree
    [else
     ; pop off the last element recursively
     (define-values (new-root lifted-tail) (pvector-node-pop root (sub1 tail-offset)))
     (cond
       ; if the new root node only has one element, we can discard it
       [(not (vector-ref (pvector-node-children new-root) 1))
        (pvector-head (sub1 size)
                      (vector-ref (pvector-node-children new-root) 0)
                      (pvector-leaf->tail lifted-tail))]
       [else
        (pvector-head (sub1 size)
                      new-root
                      (pvector-leaf->tail lifted-tail))])]))

;; ---------------------------------------------------------------------------------------------------

(define (make-pvector-tail [size 0] [fill #f])
  (pvector-tail size (make-vector branching-factor fill)))

(define (pvector-tail-set tail index value)
  (match-define (pvector-tail size children) tail)
  (define new-children (vector-copy children))
  (vector-set! new-children index value)
  (pvector-tail size new-children))

(define (pvector-tail-conj tail value)
  (match-define (pvector-tail size children) tail)
  (define new-children (vector-copy children))
  (vector-set! new-children size value)
  (pvector-tail (add1 size) new-children))

(define (pvector-tail-pop tail)
  (match-define (pvector-tail size children) tail)
  (define new-children (vector-copy children))
  (vector-set! new-children (sub1 size) #f)
  (pvector-tail (sub1 size) new-children))

(define (pvector-tail->leaf tail)
  (pvector-node 0 (pvector-tail-children tail)))

(define (pvector-leaf->tail node)
  (pvector-tail branching-factor (pvector-node-children node)))

(define pvector-empty
  (pvector-head 0 #f (make-pvector-tail)))

;; ---------------------------------------------------------------------------------------------------

(define (make-pvector-node height [fill #f])
  (pvector-node height (make-vector branching-factor fill)))

; gets the maximum possible capacity of a node and its children
(define (pvector-node-capacity node)
  (expt branching-factor (add1 (pvector-node-height node))))

(define (pvector-node-get node index)
  (match-define (pvector-node height children) node)
  ; extract the portion of the index to be used for this lookup
  (let* ([index-start (* height bits-per-node)]
         [index-end (* (add1 height) bits-per-node)]
         [extracted-index (bitwise-bit-field index index-start index-end)])
    (define child (vector-ref children extracted-index))
    (cond
      ; if ‘height’ is zero, this is a leaf
      [(zero? height) child]
      ; otherwise, it should be an internal node, so recur
      [(pvector-node? child) (pvector-node-get child index)]
      ; if it's #f, we shouldn't have gotten here
      [else (error 'pvector-node-get "internal out of bounds error")])))

(define (pvector-node-set node index value)
  (match-define (pvector-node height children) node)
  ; copy the data of each node we pass through
  (define new-children (vector-copy children))
  ; extract the portion of the index to be used for this lookup
  (let* ([index-start (* height bits-per-node)]
         [index-end (* (add1 height) bits-per-node)]
         [extracted-index (bitwise-bit-field index index-start index-end)])
    (define child (vector-ref children extracted-index))
    ; update the relevant value of the copied data
    (vector-set!
     new-children extracted-index
     (cond
       ; if ‘height’ is zero, this is a leaf, so we just insert the new value
       [(zero? height) value]
       ; otherwise, it should be an internal node, so recur
       [(pvector-node? child) (pvector-node-set child index value)]
       ; if it's #f, we need to build a new node
       [else (pvector-node-set (make-pvector-node (sub1 height)) index value)])))
  ; return the replacement node
  (pvector-node height new-children))

(define (pvector-node-conj node index new-node)
  (match-define (pvector-node height children) node)
  ; copy the data of each node we pass through
  (define new-children (vector-copy children))
  ; extract the portion of the index to be used for this lookup
  (let* ([index-start (* height bits-per-node)]
         [index-end (* (add1 height) bits-per-node)]
         [extracted-index (bitwise-bit-field index index-start index-end)])
    (define child (vector-ref children extracted-index))
    ; update the relevant value of the copied data
    (vector-set!
     new-children extracted-index
     (cond
       ; if ‘height’ is one, this is almost a leaf, so we just insert the new node
       [(= 1 height) new-node]
       ; otherwise, it should be an internal node, so recur
       [(pvector-node? child) (pvector-node-conj child index new-node)]
       ; if it's #f, we need to build a new node
       [else (pvector-node-conj (make-pvector-node (sub1 height)) index new-node)])))
  ; return the replacement node
  (pvector-node height new-children))

(define (pvector-node-pop node index)
  (match-define (pvector-node height children) node)
  ; extract the portion of the index to be used for this lookup
  (let* ([index-start (* height bits-per-node)]
         [index-end (* (add1 height) bits-per-node)]
         [extracted-index (bitwise-bit-field index index-start index-end)])
    (define child (vector-ref children extracted-index))
    (cond
      ; if the index to pop is zero, then we can just throw this node away
      [(and (= 1 height) (zero? extracted-index)) (values #f child)]
      [else
       ; figure out what the new child should be
       (define-values (new-value lifted-tail)
         (cond
           ; if ‘height’ is one, the child is a leaf, so we just set it to #f
           [(= 1 height) (values #f child)]
           ; otherwise, it should be an internal node, so recur
           [(pvector-node? child) (pvector-node-pop child index)]
           ; if it's #f, we shouldn't have gotten here
           [else (error 'pvector-node-pop "internal out of bounds error")]))
       (cond
         ; if the value we get back is #f and there's nothing else in this node, it can be discarded
         [(and (not new-value) (zero? extracted-index)) (values #f lifted-tail)]
         [else
          ; copy the data of each node we pass through
          (define new-children (vector-copy children))
          ; insert the new value
          (vector-set! new-children extracted-index new-value)
          ; return the replacement node
          (values (pvector-node height new-children) lifted-tail)])])))
