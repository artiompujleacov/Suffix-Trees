#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))
(require racket/stream)

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).
(define (longest-common-prefix w1 w2)
  (define (helper prefix w1 w2)
    (cond
      ((or (null? w1) (null? w2))
       (list (reverse prefix) w1 w2))
      ((and (not (null? w1)) (not (null? w2)) (char? (car w1)) (char? (car w2)) (char=? (car w1) (car w2)))
       (helper (cons (car w1) prefix) (cdr w1) (cdr w2)))
      (else
       (list (reverse prefix) w1 w2))))

  (helper '() w1 w2))

; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (if (collection-empty? (collection-rest words))
      (collection-first words)
      (let ((prefix (collection-first (longest-common-prefix (collection-first words) (collection-cadr words)))))
        (longest-common-prefix-of-collection (collection-cons prefix (collection-cddr words))))))

(define (match-pattern-with-label st pattern)
  (let ((branch (get-ch-branch st (collection-first pattern))))
    (if branch
        (let* ((label (get-branch-label branch))
               (lcp (longest-common-prefix pattern label)))
          (cond
            ((collection-empty? (collection-cadr lcp)) true)
            ((equal? (collection-first lcp) label) (list label (collection-tail pattern (length (collection-first lcp))) (get-branch-subtree branch)))
            (else (list false (collection-first lcp)))))
        (list false '()))))


(define (st-has-pattern? st pattern)
  (let loop ((pattern pattern) (st st))
    (cond
      ((collection-empty? pattern) true)
      ((match-pattern-with-label st pattern)
       => (lambda (result)
            (cond
              ((eq? result true) true)
              ((eq? (collection-first result) false) false)
              ((and (not (collection-empty? (collection-cadr result))) (not (collection-empty? (collection-caddr result))))
               (loop (collection-cadr result) (collection-caddr result))))))
      (else false))))


(define (get-suffixes text)
  (if (collection-empty? text)
      (empty-collection)
      (collection-cons text (get-suffixes (cdr text)))))


(define (get-ch-words words ch)
  (collection-filter 
    (lambda (word) 
      (and (not (collection-empty? word)) (eq? (collection-first word) ch))) 
    words))

 
(define (ast-func suffixes)
  (let ((label (list (collection-first (collection-first suffixes))))
        (new-suffixes (collection-map cdr suffixes)))
    (cons label new-suffixes)))


(define (cst-func suffixes)
  (define (common-prefix lst1 lst2)
    (if (or (collection-empty? lst1) (collection-empty? lst2) (not (eq? (collection-first lst1) (collection-first lst2))))
        '()
        (cons (collection-first lst1) (common-prefix (cdr lst1) (cdr lst2)))))
  (let* ((label (collection-foldl common-prefix (collection-first suffixes) (collection-rest suffixes)))
         (new-suffixes (collection-map (lambda (suffix) (drop suffix (length label))) suffixes)))
    (cons label new-suffixes)))

(define (substring? text pattern) 
  (st-has-pattern? (text->ast text) pattern))
; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (define (suffixes-for-char acc char)
    (let ([char-suffixes (get-ch-words suffixes char)])
      (if (collection-empty? char-suffixes)
          acc
          (collection-append acc (empty-collection char-suffixes)))))

  (define (process-labels label-suffix)
    (let* ([label (car label-suffix)]
           [suffixes (cdr label-suffix)]
           [first-suffix (collection-first suffixes)])
      (if (collection-empty? suffixes)
          (cons label empty-st)
          (if (collection-empty? first-suffix)
              (cons label empty-st)
              (cons label (suffixes->st labeling-func suffixes alphabet))))))

  (let* ([all-suffixes (collection-fold suffixes-for-char empty-st alphabet)]
         [labeled-suffixes (collection-map labeling-func all-suffixes)])
    (collection-map process-labels labeled-suffixes)))

(define text->st
  (lambda (labeling-func)
    (lambda (text)
      (let* ((suffixes (get-suffixes (append text '(#\$))))
             (alphabet (sort (remove-duplicates (append text '(#\$))) char<?)))
        (suffixes->st labeling-func suffixes alphabet)))))

(define text->ast (text->st ast-func))

(define text->cst (text->st cst-func))

(define (streamy->eager st)
  (map
   (lambda (branch) (cons (car branch) (streamy->eager (cdr branch))))
   (stream->list st)))

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.

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


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len) 'your-code-here)