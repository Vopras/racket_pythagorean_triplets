#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.
(define (dot-product X Y)
  (if (null? X)
      0
  (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y)))))


; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.
(define (multiply M V)
  (multiply-tail M V '())
  )

(define (multiply-tail M V acc)
  (if (null? M)
      acc
      (multiply-tail (cdr M) V (append acc (list (dot-product (car M) V)))))

  )


; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)


(define (max-pe-nivel nivel max)
  (if (= nivel 1)
      max
      (max-pe-nivel (- nivel 1) (+ (* max 3) 1)))
  )

(define (min-pe-nivel nivel min)
  (if (= nivel 1)
      min
      (min-pe-nivel (- nivel 1) (- (* min 3) 1)))
  )

;(max-pe-nivel 3 1)
;(min-pe-nivel 3 1)



(define (pe-ce-nivel n min max nivel)
  (if (and (>= n min) (<= n max))
      nivel
      (pe-ce-nivel n (min-pe-nivel (+ 1 nivel) 1) (max-pe-nivel (+ 1 nivel) 1) (+ nivel 1))
))

;(pe-ce-nivel 6 1 1 1)

(define (between n a b)
  (if (and (>= n a) (<= n b))
      1
      0))
;(between 2 4 6)
;(between 5 4 6)
;(between 7 4 6)

(define (unu-pe-trei a b)
  (+ (- (/ (+ (- b a) 1) 3) 1) a))

;(unu-pe-trei 2 4)
;(unu-pe-trei 5 13)
;(unu-pe-trei 14 40)

(define (doi-pe-trei a b)
  (+ (- (* (/ (+ (- b a) 1) 3) 2) 1) a))

;(doi-pe-trei 2 4)
;(doi-pe-trei 5 13)
;(doi-pe-trei 14 40)


(define (a-cata-treime n min max)
  (cond ((= (between n min (unu-pe-trei min max)) 1) 1)
        ((= (between n (+ (unu-pe-trei min max) 1) (doi-pe-trei min max)) 1) 2)
        (else 3)))


;(a-cata-treime 22 14 40)
;(a-cata-treime 23 14 40)
;(a-cata-treime 31 14 40)

(define (get-transformations-aux3 n acc min max)
  (if (> 3 (- max min))
       (append acc (list (a-cata-treime n min max)))
      (cond ((= 1 (a-cata-treime n min max)) (get-transformations-aux3 n (append acc '(1)) min (unu-pe-trei min max)))
            ((= 2 (a-cata-treime n min max)) (get-transformations-aux3 n (append acc '(2)) (+ (unu-pe-trei min max) 1) (doi-pe-trei min max)))
            (else (get-transformations-aux3 n (append acc '(3)) (+ (doi-pe-trei min max) 1) max)))
  ))

;(get-transformations-aux3 25 '() 14 40)

;(display (= 1 (a-cata-treime 3 2 4)))

(define (get-transformations n)
  (if (= 1 n)
      '()
      (get-transformations-aux3 n '() (min-pe-nivel (pe-ce-nivel n 1 1 1) 1) (max-pe-nivel (pe-ce-nivel n 1 1 1) 1))
      
  ))


; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.
(define (apply-matrix-transformations Ts ppt)
  (if (null? Ts)
      ppt
      (cond ((= (car Ts) 1) (apply-matrix-transformations (cdr Ts) (multiply T1 ppt)))
            ((= (car Ts) 2) (apply-matrix-transformations (cdr Ts) (multiply T2 ppt)))
            (else (apply-matrix-transformations (cdr Ts) (multiply T3 ppt)))
             )))


; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5)))

(get-nth-ppt-from-matrix-transformations 64)
