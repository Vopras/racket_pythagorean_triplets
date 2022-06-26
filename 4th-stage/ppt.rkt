#lang racket

(provide (all-defined-out))

;; Dacă ne interesează doar al n-lea TPP din arbore, este
;; convenabil să determinăm secvența de transformări care
;; conduce la acest TPP, așa cum am procedat până acum.
;;
;; În schimb, dacă ne interesează primele n TPP (sau în
;; general o secvență mai lungă de TPP) ar fi de preferat
;; crearea unui flux infinit care să le conțină pe toate
;; în ordine.
;;
;; Observăm că această ordine corespunde unei parcurgeri
;; BFS a arborelui infinit. Acesta este un BFS mai simplu
;; decât BFS-ul uzual
;; (https://en.wikipedia.org/wiki/Breadth-first_search),
;; întrucât succesorii unui TPP sunt automat triplete noi,
;; deci nu este necesar să verificăm dacă un nod a mai
;; fost sau nu vizitat.
;; 
;; Schema acestui BFS simplificat este:
;;  1. inițializăm coada de noduri care trebuie vizitate cu
;;     rădăcina arborelui (tripletul (3,4,5))
;;  2. adăugăm primul nod din coadă în rezultat
;;  3. adăugăm cei 3 succesori ai săi în coada de noduri
;;     care trebuie vizitate
;;  4. revenim la pasul 2 (întrucât construim un flux
;;     infinit, nu există condiție de oprire, și toate
;;     structurile sunt fluxuri: atât coada cât și
;;     rezultatul funcției BFS)

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Aduceți aici (nu sunt necesare modificări) implementările
; funcțiilor dot-product și multiply din etapa 1 sau 2.
; Cele două funcții nu sunt re-punctate de checker, însă 
; sunt necesare generării succesorilor unui nod.
(define (dot-product X Y)
  (if (null? X)
      0
  (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y)))))

(define (multiply M V)
  (multiply-tail M V '())
  )

(define (multiply-tail M V acc)
  (if (null? M)
      acc
      (multiply-tail (cdr M) V (append acc (list (dot-product (car M) V)))))

  )

; TODO
; Definiți fluxul infinit de TPP folosind algoritmul descris
; (parcurgerea BFS a arborelui infinit).
; Funcție utilă: stream-append
; Folosiți cel puțin o formă de let.

(define naturals
  (let nat ([seed 0])
    (stream-cons seed (nat (add1 seed)))))

(define (update-q q)
  (let (
        (c1 (multiply T1 (car q)))
        (c2 (multiply T2 (car q)))
        (c3 (multiply T3 (car q)))
        )
    (cond
      ((and (not(member c1 q)) (and (not(member c2 q )) (not(member c3 q ))))   (append (cdr q) (cons c1 (cons c2 (list c3)))))
      ((and (not(member c1 q)) (and (not(member c2 q )) (member c3 q )))        (append (cdr q) (cons c1 (cons c2 '()))))
      ((and (not(member c1 q)) (and (member c2 q)       (not(member c3 q ))))   (append (cdr q) (cons c1 (cons c3 '()))))
      ((and (not(member c1 q)) (and (member c2 q)       (member c3 q )))        (append (cdr q) (cons c1 '())))
      ((and (member c1 q)      (and (not(member c2 q))  (not(member c3 q ))))   (append (cdr q) (cons c2 (cons c3 '()))))
      ((and (member c1 q)      (and (not(member c2 q))  (member c3 q )))        (append (cdr q) (cons c2 '())))
      ((and (member c1 q)      (and (member c2 q)       (not(member c3 q ))))   (append (cdr q) (cons c3 '())))
      ((and (member c1 q)      (and (member c2 q)       (member c3 q )))         (cdr q))
      (else 'error)
    
      )
    )
  )

;(update-q (list '(15 8 17) '(21 20 29) '(5 12 13)))

(define squares
  (stream-map sqr naturals))

;(stream->list (stream-take (stream-append naturals squares) 100))

(define (make-ppt q)
  (stream-cons (car q) (make-ppt (update-q q)))
  )

(define ppt-stream-in-tree-order
  (let
      (
       (q (list'(3 4 5)))
       )
    (make-ppt q)
    )
  )
;ppt-stream-in-tree-order


;(stream->list (stream-take ppt-stream-in-tree-order 5))

;; Un alt mod de a genera TPP se folosește de perechi (g, h)
;; care indeplinesc condițiile:
;;    g, h impare
;;    g < h
;;    g, h prime între ele
;;
;; Nu întâmplător am ales aceste notații, teoria este aceeași
;; cu cea din spatele cvartetelor (g, e, f, h), pe care le
;; putem exprima și ca (g, (h-g)/2, (h+g)/2, h).
;;
;; Pentru a obține un TPP dintr-o pereche (g, h) se aplică
;; aceleași formule (dar le vom exprima în funcție de g și h):
;;    a = gh
;;    b = 2ef = (h - g)(h + g) / 2
;;      = (h^2 - g^2) / 2
;;    c = e^2 + f^2 = (h - g)^2 / 4 + (h + g)^2 / 4
;;      = (h^2 + g^2) / 2
;;
;; Acest mod de generare ne furnizează TPP în altă ordine
;; decât cea dată de parcurgerea în lățime a arborelui TPP.
;;
;; Noua ordine se obține parcurgând pe coloane diagrama:
;;                        h      
;;         3     5     7     9     11   .  .  .
;;    1  (1,3) (1,5) (1,7) (1,9) (1,11) .  .  .
;;    3        (3,5) (3,7)   -   (3,11) .  .  .
;;    5              (5,7) (5,9) (5,11) .  .  .
;; g  7                    (7,9) (7,11) .  .  .
;;    9                          (9,11) .  .  .
;;    .                                 .  .  .
;;    .                                    .  .
;;    .                                       .
;; (lipsește perechea (3,9), 3 și 9 nefiind prime între ele)
;;
;; Folosind această indexare, primele 6 TPP sunt:
;;    (3,4,5)                           - din perechea (1,3)
;;    (5,12,13), (15,8,17)              - din (1,5), (3,5)
;;    (7,24,25), (21,20,29), (35,12,37) - din (1,7), (3,7), (5,7)
;;
;; Ne propunem să definim fluxul infinit de TPP în ordinea de
;; mai sus. Acesta se bazează pe fluxul corespunzător de 
;; perechi (g, h), pe care îl generăm astfel:
;;  - pornim cu 2 fluxuri infinite:
;;    * G = 1, 3, 5, 7 ...
;;    * H = 3, 5, 7, 9 ... (întrucât g < h)
;;  - fluxul ordonat pe coloane va conține:
;;    * perechea compusă din cele mai mici numere din G și H
;;      (ex: (1,3))
;;    * apoi interclasarea (conform ordinii "pe coloane") între:
;;      - perechile compuse dintre minimul din G și restul din H
;;        (ex: (1,5), (1,7), (1,9) ...)
;;      - fluxul ordonat generat de restul lui G și restul lui H
;;        (ex: (3,5), (3,7), (5,7) ...)
;; Aceasta este abordarea generală, în urma căreia generăm toate
;; perechile, inclusiv pe cele de numere care nu sunt prime  
;; între ele. Perechile neconforme trebuie înlăturate ulterior
;; (utilizând funcția de bibliotecă gcd).


; TODO
; Definiți o funcție care primește 2 fluxuri numerice infinite
; G și H, și generează fluxul de perechi de câte un element 
; din G și unul din H ordonate conform metodei de mai sus.
; Condițiile ca g și h să fie impare, prime între ele, respectiv
; menținerea restricției g < h (cât timp urmați algoritmul) nu
; trebuie impuse în implementarea funcției pairs.
; Ele vor fi asigurate de definirea fluxurilor de mai jos prin:
;  - apelarea lui pairs exclusiv pe fluxurile
;    G = 1, 3, 5, 7 ... și H = 3, 5, 7, 9 ...
;  - eliminarea perechilor de numere neprime între ele (care 
;    există în rezultatul funcției pairs, dar nu vor mai exista
;    în fluxul gh-pairs-stream)


(define (pairs-aux G H F)
  (let (
        (g (stream-first G))
        (h (stream-first H))
        )
    (if (= g h)
        (pairs-aux G (stream-rest H) F)
        (pairs-aux (stream-rest G) H (stream-cons (append (list g) h) F))
        )
    )
  )

(define (flux-maker G H Gs )
  (let (
        (g (stream-first G))
        (h (stream-first H))
        )
    (if (= g h)
        (flux-maker Gs (stream-rest H) Gs)
        (stream-cons (append (list g) h) (flux-maker (stream-rest G) H Gs))
        )

    )
  )



(define (pairs2 G H)
  ;(pairs-aux G H empty-stream)
  (flux-maker G H G)
  )


(define (flux-maker2 G H c comp)
  (if (> comp c)
      (stream-cons (cons (stream-ref H c) (stream-first G)) (flux-maker2 G H (add1 c) comp))
      (flux-maker2 (stream-rest G) H 0 (add1 comp))
      )
  )

(define (pairs G H)
  (flux-maker2 H G 0 1) 
  )


        
; TODO
; Definiți fluxul de perechi (g, h) pe care se bazează noua
; indexare a TPP.
; Nu folosiți recursivitate explicită (decât pentru a genera
; fluxurile de pornire - G și H).

(define (make-odd-flux n)
  (stream-cons n (make-odd-flux (+ 2 n)))
  )

(define odd-flux
  (λ (n)
    (make-odd-flux n)
    )
  )

;(stream->list (stream-take (odd-flux 3) 5))

(define (flux-maker3 s1 s2 ss)
  1
  )


(define (check-bs2 x)
  ;(if (and (= (remainder (cdr x ) (car x)) 0) (and (odd? (car x)) (odd? (cdr x))))
  (if (and (odd? (car x)) (odd? (cdr x)))
      (if (not (= (remainder (cdr x) (car x)) 0))
          #t
          #f)
      #f)
  )

(define (check-bs3 x)
  ;(if (and (= (remainder (cdr x ) (car x)) 0) (and (odd? (car x)) (odd? (cdr x))))
  (if (and (odd? (car x)) (odd? (cdr x)))
      (if (not (= (car x) 1))
          (if (not (= (remainder (cdr x) (car x)) 0))
              #t
              #f)
          #t
          ) 
      #f)
  )

(define (check-bs x)
  (if (= 1 (gcd (car x) (cdr x)))
      #t
      #f
      )
  )

(define gh-pairs-stream
  ;(flux-maker3 (odd-flux 1) (odd-flux3) (odd-flux 1))
  ;(stream-filter (λ (x) (not (= (remainder (cdr x) (car x))0))) (pairs (odd-flux 1) (odd-flux 3)))
  (stream-filter check-bs (pairs (odd-flux 1) (odd-flux 3)))
  ;(stream-filter (λ (x) (not (= (remainder (cdr x) (car x))0))) (flux-maker  (odd-flux 1) (odd-flux 3) (odd-flux 1)))
  ;(stream-filter (λ (x) (not (= (remainder (cdr x) (car x))0))) (pairs (odd-flux 1) (odd-flux 3)))
  )



; TODO
; Definiți fluxul de TPP corespunzător fluxului anterior de
; perechi (g, h).

;;    a = gh
;;    b = 2ef = (h - g)(h + g) / 2
;;      = (h^2 - g^2) / 2
;;    c = e^2 + f^2 = (h - g)^2 / 4 + (h + g)^2 / 4
;;      = (h^2 + g^2) / 2

(define (transform x)
  (let (
        (g (car x))
        (h (cdr x))
        )
    (list (* g h) (/ (- (sqr h) (sqr g)) 2) (/ (+ (sqr h) (sqr g)) 2))
    )
  )

(define ppt-stream-in-pair-order
  (stream-map transform gh-pairs-stream)
   )


