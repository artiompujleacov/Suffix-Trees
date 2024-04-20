#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

(define (substring? text pattern) 
  (st-has-pattern? (text->ast text) pattern))

(define (longest-common-substring text1 text2)
  (let* ((st1 (text->ast text1))
         (len2 (length text2))
         (max-len 0)
         (max-substr '()))
    (let loop ((i len2))
      (when (> i 0)
        (for ([j (in-range (- len2 i -1))])
          (let* ([substr (take (list-tail text2 j) i)]
                 [match (st-has-pattern? st1 substr)])
            (when match
              (let ((match-length (length substr)))
                (when (> match-length max-len)
                  (set! max-len match-length)
                  (set! max-substr substr))))))
        (loop (- i 1))))
    max-substr))

(define (internal-node? st)
  (and (not (st-empty? st)) (> (length (other-branches st)) 0)))

(define (repeated-substring-of-given-length text len)
  (define (find-repeated-substring st path)
    (if (and (>= (length path) len) (internal-node? st))
        (take path len)
        (let loop ((branches (if (st-empty? st) '() (cons (first-branch st) (other-branches st)))))
          (if (null? branches)
              #f
              (let* ((branch (car branches))
                     (label (get-branch-label branch))
                     (subtree (get-branch-subtree branch))
                     (new-path (append path label)))
                (or (find-repeated-substring subtree new-path)
                    (loop (cdr branches))))))))
  (define st (text->cst text))
  (find-repeated-substring st '()))







