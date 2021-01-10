;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ArctanPi-Learnweb) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Signatur: reihe : lambda number -> number

;; Zweck: Berechnet die Reihe a(0) + a(1) + a(2) + ...
;;    solange bis |a(i)| < eps. Dabei ist a ein Lambda-Ausdruck,
;;    der eine Zahl entgegennimmt und eine Zahl zurueckgibt.

;; Beispiele:
;;  (reihe (lambda (n) (expt 1/2 n)) 1e-6) sollte etwa 2 ergeben.
;;  (reihe (lambda (n) (expt 1/10 n)) 1e-8) sollte etwa 10/9 ergeben.
;;  (reihe (lambda (n) (/ 1 (* (+ n 1) (+ n 1)))) 1e-8) sollte etwa pi^2/6 ergeben.

;; Definition:
(define (reihe a eps)
  (ireihe 0 a 0 eps))

;; Hilfsfunktion:

(define (ireihe gemerkt a anfang eps)
  (if
   (< (a anfang)
      eps)
   gemerkt
   (ireihe (+ gemerkt (a anfang)) a (+ 1 anfang) eps)))

;; Tests:
(check-within (reihe (lambda (n) (expt 1/2 n)) 1e-6) 2 1e-3)
(check-within (reihe (lambda (n) (expt 1/10 n)) 1e-8) 10/9 1e-5)
(check-within (reihe (lambda (n) (/ 1 (* (+ n 1) (+ n 1)))) 1e-8) (/ (* pi pi) 6) 1e-4)



;; Signatur: approx-pi : [ergaenzen]

;; Zweck: Berechnet einen Naehrungswert fuer pi mittels Machin-Formel.
;; Beispiele: 
;; Definition:
;;(define (approx-pi ...)
;;  ...)

;; Tests: [ergaenzen]

;; Hilfsfunktionen:
(define (arctan x)
  (reihe
   (lambda (n)
    (*
     (expt (- 0 1) n)
     (/
      (expt x (+ (* 2 n) 1))
      (+ (* 2 n) 1)
      ))) 1e-500))
