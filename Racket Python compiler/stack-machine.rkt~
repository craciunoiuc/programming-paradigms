#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-varnames)
(provide empty-stack)


; Alegeți metoda de reprezentarea a unei stive.
; Implementați :
(define empty-stack 'your-code-here)
(define (make-stack) 'your-code-here)

(define push 'your-code-here)
(define top 'your-code-here)
(define pop 'your-code-here)

; Alegeți metoda de reprezentare a unei mașini stivă.
; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  'your-code-here)

(define get-varnames 'your-code-here)
(define get-consts 'your-code-here)
(define get-names 'your-code-here)
(define get-code 'your-code-here)

;; Întoarce stiva de execuție
(define get-stack 'your-code-here)

;; Întoarce instruction counterul.
(define get-IC 'your-code-here)



(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  'your-code-here)

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine 'CO-VARNAMES "new-varnames" stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine 'CO-NAMES "new-names" stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  'your-code-here)

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  'your-code-here)

;; Definiți funcția pop-exec-stack care primește o masină stivă
;; și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  'your-code-here)


;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.
(define (run-stack-machine stack-machine)
  'your-code-here)
