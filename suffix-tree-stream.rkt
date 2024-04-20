#lang racket

(provide (all-defined-out))
(require "collection.rkt")
(require racket/stream)

(define empty-st (stream))

(define (st-empty? st)
  (equal? st empty-st))

(define first-branch (lambda (st) (collection-first st)))

(define other-branches (lambda (st) (collection-rest st)))

(define get-branch-label (lambda (branch) (collection-first branch)))

(define get-branch-subtree (lambda (branch) (collection-rest branch)))

(define (get-ch-branch st ch)   
  (cond [(null? st) #f]
        [else (let ([branch (collection-first st)])
                (if (equal? (car (car branch)) ch)
                    branch
                    (get-ch-branch (collection-rest st) ch)))]))
