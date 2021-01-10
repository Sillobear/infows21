;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Nachfolgertupel) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ---------------------------------------------------------------------
;; Aufgabenteil (a)
;; ---------------------------------------------------------------------

;; Signatur: nachfolger : list-of-numbers -> list-of-numbers
;; Zweck: Berechnet das Nachfolgertupel des bereits vorhandenen Tupels
;; Beispiele:
;;    (nachfolger (list 2 6 7 11)) sollte (list 4 1 4 9) ergeben.
;;    (nachfolger empty) sollte empty ergeben.
;;    (nachfolger (list 42)) sollte (list 0) ergeben.
;;    (nachfolger (list 13 37 42)) sollte (list 24 5 29) ergeben.
;;    (nachfolger (list 0 0 0 0 0)) sollte (list 0 0 0 0 0) ergeben.
;; Schablone:
;;  Signatur: list-of-numbers -> ???
;; (define (nachfolger t)
;; (cond
;; [(empty? t)...]
;; [else ... (t) ...
;;       ... (Hilfsfunktion (rest t))]
;;))
;; Definition:

(define (nachfolger t)
  (cond
    [(empty?  t) empty]                                                            ;; Falls die Liste Leer ist wird automatisch empty zurückgegeben (Dies ist auch das Nachfolgertupel)
    [else (Berechnung t (append (rest t) (list(first t))))]                          ;; In jedem anderen Fall ( der gegebenen Tests) wird Berechnung aufgerufen (mit der Liste) aufgerufen
                                                                                      ;; (list 2 6 7 11) --> (list 6 7 11 2) --> Verschoben ist hier die Liste mit dem ersten und letzten Element getauscht
                                                       ))
  
  

;; Tests:
(check-expect (nachfolger (list 2 6 7 11)) (list 4 1 4 9))
(check-expect (nachfolger empty)           empty)
(check-expect (nachfolger (list 42))       (list 0))
(check-expect (nachfolger (list 13 37 42)) (list 24 5 29))
(check-expect (nachfolger (list 0 0 0 0))  (list 0 0 0 0))


;; Hier Hilfsfunktionen definieren.
;;Signatur: list-of-numbers -> list of numbers
;;Berechnet den Abstrakten Wert der 1. Zahl der normalen und der versetzten Reihe. 
(define (Berechnung t r)
(cond
  [(empty?  t) empty]                                                     ;; Stellt hier eine Abbruchbedingung der Rekursionschleife dar!
  [else (cons                                                             ;; Wird in jedem Fall ausgeführt (ausser beim vorzeitigen Abbruch)
         (abs(- (first t) (first r)))                                     ;; Funktionsweise der Schleife  (cons a (cons b (cons c (cons d empty))))  ---> (list a b c d)
         (Berechnung (rest t) (rest r) ))]))



;; ---------------------------------------------------------------------
;; Aufgabenteil (b)
;; ---------------------------------------------------------------------

;; Signatur: nachfolgerstapel : list-of-numbers -> list-of-list-of-numbers
;; Zweck: Erzeugt Liste der Nachfolgertupel eines Tupels t.
;; Beispiele:
;;    (nachfolgerstapel (list 2 6 7 11)) sollte
;;      (list (list 2 6 7 11) (list 4 1 4 9) (list 3 3 5 5)
;;            (list 0 2 0  2) (list 2 2 2 2) (list 0 0 0 0)) ergeben.
;;    (nachfolgerstapel (list 0)) sollte (list (list 0)) ergeben.
;;    (nachfolgerstapel (list 23 41 11 18)) sollte
;;      (list (list 23 41 11 18) (list 18 30  7  5) (list 12 23 2 13)
;;            (list 11 21 11  1) (list 10 10 10 10) (list  0  0 0  0)) ergeben.
;;    (nachfolgerstapel empty) sollte einen Fehler erzeugen.
;;    (nachfolgerstapel (list 1 1 1)) sollte einen Fehler erzeugen.
;; Schablone: 
;;  Signatur: nachfolgerstapel: list-of-numbers -> ??
;;  (define (nachfolgerstapel t)
;;  (cond
;;  [(empty? t)...]
;;  [else ... (first t) ...
;;        ... (nachfolgerstapel (t))]

;; Definition: 
(define (nachfolgerstapel t)
  (cond
    [(empty? t)(error "nachfolgerstapel: Tupel ungueltiger Laenge uebergeben.")]
     [(not(integer? (log(length t)2)))(error "nachfolgerstapel: Tupel ungueltiger Laenge uebergeben.")]  
     [else (naechstestupel t)]))




;; Tests: 
(check-expect (nachfolgerstapel (list 2 6 7 11))
   (list (list 2 6 7 11) (list 4 1 4 9) (list 3 3 5 5)
         (list 0 2 0  2) (list 2 2 2 2) (list 0 0 0 0)))
(check-expect (nachfolgerstapel (list 0))
   (list (list 0)))
(check-expect (nachfolgerstapel (list 23 41 11 18))
   (list (list 23 41 11 18) (list 18 30  7  5) (list 12 23 2 13)
         (list 11 21 11  1) (list 10 10 10 10) (list  0  0 0  0)))
(check-error (nachfolgerstapel empty) "nachfolgerstapel: Tupel ungueltiger Laenge uebergeben.")
(check-error (nachfolgerstapel (list 1 1 1)) "nachfolgerstapel: Tupel ungueltiger Laenge uebergeben.")

;; Hier Hilfsfunktionen definieren.


;;Signatur: nulltupel: list-of-numbers -> integer
;;Überprüft ob die Liste eine Nullzeilen Liste ist und gibt dementsprechend einen Wert zurück              
(define (nulltupel t)
  (cond
    [(empty? t) 0]
    [(= 0 (+ (first t) (nulltupel(rest t))))0]
    [else 1]))


;;Signatur: nächtestupel: list-of-numbers -> list-of-list-of-numbers
;;Erstellt eine Tupelliste
(define (naechstestupel t)
  (cond
    [(= 0(nulltupel t))(list t)]
    [else ( append (list t) (naechstestupel (nachfolger t)))]))





;; (C)


;; (A)  (Berechnung t r) ist linear Rekursiv da diese sich jedesmal höchstens einmal selbst aufruft
;; (B)   (Nulltupel) ist hier rekursiv da (gleiche Begründung)
         ;; (naechstestupel ) ruft einen Iterativen Prozess als auch einen rekursiven Prozess auf, da diese eine rekursion mit einer iteration (nachfolger) aufruft.
         ;; 
