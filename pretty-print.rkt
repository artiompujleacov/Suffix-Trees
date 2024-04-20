#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))


(define (enumerate l) (map cons (range (length l)) l))

(define (pretty-print st)
  (define (make-indent lasts)
    (if (null? lasts) ""
        (let ([prev-last (take lasts (sub1 (length lasts)))]
              [curr-last (last lasts)])
          (string-append
           (apply string-append (map (lambda (is-last) (if is-last "    " "│   ")) prev-last))
           (if curr-last "└───" "├───")))))

  (define (print-label lasts str)
    (format "~a~a\n" (make-indent lasts) str))

  (define (helper lasts tree)
    (let* ([label (list->string (get-branch-label tree))]
           [subs (get-branch-subtree tree)]
           [last-index (sub1 (length subs))])

      (apply string-append
             (print-label lasts label)
             (map (lambda (pair)
                    (match-let* ([(cons index branch) pair]
                                 [next-lasts (append lasts (list (= index last-index)))])
                      (helper next-lasts branch)))
                  (enumerate subs)))))
  
  (define augmented-tree (cons (string->list "*") st))

  (helper '() augmented-tree))


; ST pentru "banana".
(define stree-1
  '(((#\$))
    ((#\a) ((#\$))
           ((#\n #\a) ((#\$))
                      ((#\n #\a #\$))))
    ((#\b #\a #\n #\a #\n #\a #\$))
    ((#\n #\a) ((#\$))
               ((#\n #\a #\$)))))

; ST pentru sufixele nevide ale lui "abb".
(define stree-2
  '(((#\a #\b #\b #\$))
    ((#\b) ((#\$))
           ((#\b #\$)))))

(displayln (pretty-print '(((#\a)))))
(displayln (pretty-print '(((#\b) ((#\b #\$)) ((#\$))))))
(displayln (pretty-print stree-1))
(displayln (pretty-print stree-2))
