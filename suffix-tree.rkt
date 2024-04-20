#lang racket

(provide (all-defined-out))
;;
;;                            *
;;   __________ ______________|______________
;;  |          |              |              |
;;  $          A           BANANA$           NA
;;  |          |              |              |
;;  *          *              *              *
;;       ______|______                 ______|______
;;      |             |               |             |
;;      $             NA              $            NA$
;;      |             |               |             |        
;;      *             *               *             *
;;                 ___|___
;;                |       |
;;                $      NA$
;;                |       |
;;                *       *
;;                        
;;                            *
;;   __________ ______________|______________
;;  |          |              |              |
;;  $          A              B              N
;;  |          |              |              |
;;  *          *              *              *
;;         ____|____          |              |
;;        |         |         |              |
;;        $         N         A              A
;;        |         |         |              |        
;;        *         *         *              *
;;                  |         |          ____|____
;;                  |         |         |         |
;;                  A         N         $         N
;;                  |         |         |         |
;;                  *         *         *         *
;;                __|__       |                   |
;;               |     |      |                   |
;;               $     N      A                   A
;;               |     |      |                   |
;;               *     *      *                   *
;;                     |      |                   |
;;                     A      N                   $
;;                     |      |                   |
;;                     *      *                   *
;;                     |      |
;;                     $      A
;;                     |      |
;;                     *      *
;;                            |
;;                            $
;;                            |
;;                            *
(define empty-st '())

(define (st-empty? st)
  (equal? st empty-st))

(define first-branch (lambda (st) (car st)))

(define other-branches (lambda (st) (cdr st)))

(define get-branch-label (lambda (branch) (car branch)))

(define get-branch-subtree (lambda (branch) (cdr branch)))

(define (get-ch-branch st ch)   (cond [(st-empty? st) #f]
  [else (let ([branch (first-branch st)])
    (if (equal? (car (get-branch-label branch)) ch)
      branch
        (get-ch-branch (other-branches st) ch)))])) 