#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")

(provide (all-defined-out))


(define (get-suffixes text)
  (if (null? text)
      '()
      (cons text (get-suffixes (cdr text)))))

(define (get-ch-words words ch)
  (filter (lambda (word) (and (not (null? word)) (eq? (car word) ch))) words))

(define (ast-func suffixes)
  (let ((label (list (car (car suffixes))))
        (new-suffixes (map cdr suffixes)))
    (cons label new-suffixes)))

(define (cst-func suffixes)
  (define (common-prefix lst1 lst2)
    (if (or (null? lst1) (null? lst2) (not (eq? (car lst1) (car lst2))))
        '()
        (cons (car lst1) (common-prefix (cdr lst1) (cdr lst2)))))
  (let* ((label (foldl common-prefix (car suffixes) (cdr suffixes)))
         (new-suffixes (map (lambda (suffix) (drop suffix (length label))) suffixes)))
    (cons label new-suffixes)))

(define (suffixes->st labeling-func suffixes alphabet)
  (define (build-branch ch)
    (let* ((ch-suffixes (get-ch-words suffixes ch)))
      (if (null? ch-suffixes)
          '()
          (let* ((labeling-result (labeling-func ch-suffixes))
                 (label (if (null? labeling-result) '() (car labeling-result)))
                 (new-suffixes (if (null? labeling-result) '() (cdr labeling-result))))
            (cons label (suffixes->st labeling-func new-suffixes alphabet))))))
  
  (filter (lambda (branch) (not (null? branch))) (map build-branch alphabet)))

(define (text->st labeling-func)
  (lambda (text)
    (let* ((suffixes (get-suffixes (append text '(#\$))))
           (alphabet (sort (remove-duplicates (append text '(#\$))) char<?)))
      (suffixes->st labeling-func suffixes alphabet))))

(define text->ast (text->st ast-func))

(define text->cst (text->st cst-func))
