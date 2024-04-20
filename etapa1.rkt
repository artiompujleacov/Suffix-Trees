#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

(define (longest-common-prefix w1 w2)
  (define (helper prefix w1 w2)
    (cond
      ((or (null? w1) (null? w2))
       (list (reverse prefix) w1 w2))
      ((char=? (car w1) (car w2))
       (helper (cons (car w1) prefix) (cdr w1) (cdr w2)))
      (else
       (list (reverse prefix) w1 w2))))

  (helper '() w1 w2))

(define (longest-common-prefix-of-list words)
  (if (null? (cdr words))
      (car words)
      (let ((prefix (car (longest-common-prefix (car words) (cadr words)))))
        (longest-common-prefix-of-list (cons prefix (cddr words))))))

(define (match-pattern-with-label st pattern)
  (let ((branch (get-ch-branch st (car pattern))))
    (if branch
        (let* ((label (get-branch-label branch))
               (lcp (longest-common-prefix pattern label)))
          (cond
            ((null? (cadr lcp)) true)
            ((equal? (car lcp) label) (list label (list-tail pattern (length (car lcp))) (get-branch-subtree branch)))
            (else (list false (car lcp)))))
        (list false '()))))

(define (st-has-pattern? st pattern)  
  (let loop ((pattern pattern) (st st))
    (cond
      ((null? pattern) true)
      ((match-pattern-with-label st pattern)
       => (lambda (result)
            (cond
              ((eq? result true) true)
              ((eq? (car result) false) false)
              (else (loop (cadr result) (caddr result)))))
            )
      (else false))))

