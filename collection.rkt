#lang racket

(provide (all-defined-out))
(require racket/stream)

;; În acest fișier vă definiți constructorii și
;; operatorii tipului Collection.
;; În etapele anterioare, colecțiile erau de fapt
;; liste.
;; În definițiile de mai jos, veți considera că
;; o colecție este implementată ca flux.

; Întrucât stream-cons nu este o funcție obișnuită, 
; ci este definită ca o sintaxă specială, astfel
; încât ea să nu își evalueze argumentele înainte 
; de apel (comportament pe care ni-l dorim și pentru 
; collection-cons), nu putem folosi o definiție
; de tipul
;    (define collection-cons stream-cons)
; (genul acesta de definiție generează o eroare).
; Nici varianta
;    (define (collection-cons x xs) (stream-cons x xs))
; nu este o soluție, întrucât funcțiile definite de noi
; în Racket sunt funcții stricte, iar x și xs vor fi
; evaluate înainte de a intra în corpul funcției
; collection-cons și a descoperi că ele vor fi
; argumentele unui stream-cons.
; Modul de a defini collection-cons pentru a reproduce
; întocmai comportamentul lui stream-cons este:
(define-syntax-rule (collection-cons x xs) (stream-cons x xs))
; Obs: puteți schimba numele funcției, dacă nu vă
; place "collection-cons". Este o funcție folosită doar
; de voi în fișierul etapa4.rkt, nu de checker.


; TODO
; Scrieți în continuare restul definițiilor
; (care nu necesită o sintaxă specială).
(define (collection-empty? c)
  (stream-empty? c))

(define (empty-collection)
  stream)

(define (collection-first c)
  (cond 
    ((pair? c) (car c))
    (else (stream-first c))))

(define (collection-rest c)
  (cond
    ((pair? c) (stream))
    (else (stream-rest c))))
  
(define (collection-cadr c)
  (let ((rest (collection-rest c)))
    (if (collection-empty? rest)
        (stream)
        (collection-first rest))))

(define (collection-reverse c)  
  (define (reverse c acc)
    (if (collection-empty? c)
        acc
        (reverse (collection-rest c) (collection-cons (collection-first c) acc))))
  (reverse c (stream)))

(define (collection-cddr c) 
  (collection-rest (collection-rest c)))

(define (collection-tail coll index)
  (if (or (collection-empty? coll) (zero? index))
    coll
      (collection-tail (collection-rest coll) (sub1 index))))

(define (collection-caddr c)
  (collection-first (collection-rest (collection-rest c))))

(define (collection-filter pred coll)
  (if (collection-empty? coll)
      coll
      (if (pred (collection-first coll))
          (collection-cons (collection-first coll) (collection-filter pred (collection-rest coll)))
          (collection-filter pred (collection-rest coll)))) )

(define (collection-list coll)
  (if (collection-empty? coll)
      '()
      (cons (collection-first coll) (collection-list (collection-rest coll)))))

(define (collection-map f coll) 
  (if (collection-empty? coll)
      coll
      (collection-cons (f (collection-first coll)) (collection-map f (collection-rest coll)))))

(define (collection-foldl f acc coll)
  (if (collection-empty? coll)
      acc
      (collection-foldl f (f acc (collection-first coll)) (collection-rest coll))))

(define (collection-foldr f acc coll) 
  (if (collection-empty? coll)
      acc
      (f (collection-first coll) (collection-foldr f acc (collection-rest coll)))))

(define (collection-drop coll n)
  (if (or (collection-empty? coll) (zero? n))
      coll
      (collection-drop (collection-rest coll) (sub1 n))))

(define (collection-length coll)
  (collection-foldl (lambda (acc x) (add1 acc)) 0 coll))  

(define (collection-take coll n)  
  (if (or (collection-empty? coll) (zero? n))
      (stream)
      (collection-cons (collection-first coll) (collection-take (collection-rest coll) (sub1 n)))))

(define (collection-append coll1 coll2) 
  (if (collection-empty? coll1)
      coll2
      (collection-cons (collection-first coll1) (collection-append (collection-rest coll1) coll2))))  


(define (collection-apply f coll)
  (if (collection-empty? coll)
      coll
      (collection-append (f (collection-first coll)) (collection-apply f (collection-rest coll)))))

(define (collection-append* colls)
  (if (collection-empty? colls)
      (stream)
      (collection-append (collection-first colls) (collection-append* (collection-rest colls)))))

(define (collection-fold f acc coll)
  (stream-fold f acc coll))