;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |DeepReduce(Fertig)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ---------------------------------------------------------------------
;; Aufgabenteil (a)
;; ---------------------------------------------------------------------

;; Signatur: pure? : list -> bool
;; Zweck: Prueft, ob die uebergebene Liste ls keine Listen enthaelt.
;; Beispiele:
;;    (pure? empty)                 sollte  true ergeben.
;;    (pure? (list 1 'b 3 "d"))     sollte  true ergeben.
;;    (pure? (list 1 2 3 empty))    sollte false ergeben.
;;    (pure? (list (list 1 2 3 4))) sollte false ergeben.
;; Schablone:
;; Signatur: list of numbers --> ???
;; (define pure? ls)
;; (cond
;; [(empty? ls) ..... ]
;; [ (list? ls) ..... ]
;; [ else .....       ] ))
;; Definition:
(define (pure? ls)
  (cond
    [ (empty? ls) true]                       ;; Wenn die Liste empty ist enthält diese automatisch keine weitere Liste und daher folgt true
    [ (list? ls) (UeberpruefeListe ls)]       ;; Auch verschachtelte Listen sind eine Liste daher kann der Fall von einzelnen und mehreren Listen zusammengefasst werden
    [ else false]                             ;; Der einzige andere Fall ist das die liste keine list ist bzw. auch eine Zahl eingegeben werden kann dann folgt automatisch false, da es keine Liste ist.
    
                  ))
;; Signatur: ÜberorüfeListe : list -> bool
;; Zweck: Überprüft die übergebene Liste auf verschachtelte Listen als Rekursion
;; Schablone:
;; Signatur: list of numbers --> ???
;; (define UeberpruefeListe ls)
;; (cond
;; [ (empty? ls) ..... ]
;; [ (list? (first ls)) .... ]
;; [ else (UeberpruefeListe (rest ls))] ))

;; Defintion:

(define (UeberpruefeListe ls)     
  (cond
    [ (empty? ls) true]                       ;; Abbruchbedingung tritt nur ein wenn die Liste keine Liste enthält
    [ (list? (first ls))  false]              ;; Tritt immer dann auf wenn die Liste verschachtelt ist also eine Liste eine Liste enthält (empty ist hier auch eine Liste)
    [ else (UeberpruefeListe (rest ls))]))    ;; Überprüft die Liste solange bis die ersten zwei Befehle ein Ergebnis bringen.
  
                       


;; Tests:
(check-expect (pure? empty) true)
(check-expect (pure? (list 1 'b 3 "d")) true)
(check-expect (pure? (list 1 2 3 empty)) false)
(check-expect (pure? (list (list 1 2 3 4))) false)
(check-expect (pure? list) false) ;; Selbst hinzugefügt
(check-expect (pure? "Hallo") false) ;; Selbst hinzugefügt

;; ---------------------------------------------------------------------
;; Aufgabenteil (b)
;; ---------------------------------------------------------------------

;; Signatur: deep-reduce : list -> number
;; Zweck: Aggregiert eine verschachtelte Liste ls, die nur Zahlen enthaelt,
;;    mittels der Aggregatfunktion fun.
;; Beispiele:
;;    (deep-reduce + 42 empty) sollte 42 ergeben.
;;    (deep-reduce + 0 (list 1 2 3 4)) sollte 10 ergeben.
;;    (deep-reduce + 0 (list 1 2 (list 3 (list 4 5) 6) 7 (list (list 8)) empty 9)) sollte 45 ergeben.
;;    (deep-reduce * 1 (list (list 1 2 3 4))) sollte 24 ergeben.
;; Schablone:
;; Signatur: list of numbers --> number
;; (define deep-reduce fun neutral ls)
;; (cond
;; [ (empty? ls) ..... ]
;; [ (list? ls) (Berechnedeep fun neutral (.... ls)) ]
;; [ else (......)] ))





;; Definition:
(define (deep-reduce fun neutral ls)
  (cond
    [ (empty? ls) (fun neutral)]                                      ;; Ist die Liste Leer wird direkt mit dem Operator und der Zahl zur Berechnung fortgefahren
    [ (list? ls) (Berechnedeep fun neutral (my-flatten2 ls))]      ;; Falls es sich tatsälich um eine nicht leere Liste handelt kann diese Ausgewertet werden. Dabei wird direkt my-flatten 2 mit der
                                                               ;; übergebenen Liste aufgerufen, dadurch wird diese direkt zu einer nicht verschachtelten Liste zusammengefügt und kann daher einfach
                                                              ;; einfach berechnet werden.
    [ else ("Es ist ein Fehler aufgetreten")]                         ;; Else Condition falls die Eingabe Ungueltig ist
                                                      ))



;; Signatur: Operator Number List --> ???
;; Zweck: Berechnet den Wert der Liste mit dem gegebenen Operator und der gegebenen Zahl
;; Definition

 (define (Berechnedeep fun neutral ls)
      (cond
        [(empty? ls) (fun neutral)]                                            ;; Abbruchbedingung für die Rekursion
        [ (list? ls) (fun (first ls) (Berechnedeep fun neutral (rest ls)))]    ;; Es handelt sich hier schon um eine NICHT verschachtelte Liste da zuvor my-flatten2 angewendet wurde!
                                                                               ;; Hier werden wie in den vorigen Aufgaben einfach eine Rechnung nach dem gegebenen Operator durchgeführt



        [ else ("Es ist ein Fehler aufgetreten")] ))                           ;; Sollte keins dieser Fälle auftreten gibt es wohl einen Fehler



  
;; Signatur: List --> List
;; Zweck: Übernimmt die Aufgabe eine verschachtelte Listen zu "Flatten" d.h. diese wird zu einer nicht verschachtelten Liste zusammengefasst
;; Definition 

(define (my-flatten2 L)
    (cond
      [(null? L) '()]                                            ;; Stellt die Abbrichbedingung für die Rekursion dar.                                       
        [(list? (first L))                                       ;;  Falls first von L eine Liste ist wird
         
           (append (my-flatten2 (first L))                       ;;  append ruft hier die rekursion mit first der liste auf verbunden mit
                                                                ;; der rekursion für den rest der Liste
                   (my-flatten2 (rest L)))]                      
           
      [else (cons (first L) (my-flatten2 (rest L)))]))     ;; Ist first L keine Liste also z.b. (list 1 2 (list 1 2 3)) wird eine Liste mit dem ersten Glied der Liste und
                                                           ;; die Rekursion mit dem Rest der Liste aufgerufen.


;;Tests                                                                                  
(check-expect (deep-reduce + 42 empty) 42)
(check-expect (deep-reduce + 0 (list 1 2 3 4)) 10)
(check-expect (deep-reduce + 0 (list 1 2 (list 3 (list 4 5) 6) 7 (list (list 8)) empty 9)) 45)
(check-expect (deep-reduce * 1 (list (list 1 2 3 4))) 24)






;; (C)
;; Eine Funktion ist dann linear rekursiv wenn in jedem Zweig der if-else Schleife höchstens ein Selbstaufruf auftritt und ihre Aufrufstruktur linear ist.
;; --> Daraus folgt, dass Deep-Reduce genau dann linear Rekursiv ist wenn eine Liste aufgerufen wird die nicht verschachtelt, leer oder garkeine Liste ist.



  
          