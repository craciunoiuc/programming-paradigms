#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; Definire stiva
(define empty-stack '())
(define (make-stack) empty-stack)

(define push (lambda (elem stack) (cons elem stack)))
(define top (lambda (stack) (
                             if (null? stack)
                                null
                                (car stack))))
(define pop (lambda (stack) (
                             if (null? stack)
                                null
                                (cdr stack))))

;; Functie ce construieste masina stiva, facand o lista cu elementele
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC))

;; Functii ce preiau diferite elemente din masina stiva
(define get-varnames (lambda (stack-machine) (cadr stack-machine)))
(define get-consts (lambda (stack-machine) (caddr stack-machine)))
(define get-names (lambda (stack-machine) (cadddr stack-machine)))
(define get-code (lambda (stack-machine) (car (cddddr stack-machine))))
(define get-stack (lambda (stack-machine) (car stack-machine)))
(define get-IC (lambda (stack-machine) (car (cdr (cddddr stack-machine)))))

;; Verifica ce element din masina stiva se vrea inlocuit si apoi il schimba
;; cu cel nou primit ca parametru
(define (update-stack-machine item symbol stack-machine)
  (cond
    [(equal? symbol 'STACK) (make-stack-machine item
                                                (get-varnames stack-machine)
                                                (get-consts stack-machine)
                                                (get-names stack-machine)
                                                (get-code stack-machine)
                                                (get-IC stack-machine))]
    [(equal? symbol 'CO-VARNAMES) (make-stack-machine (get-stack stack-machine)
                                                      item
                                                      (get-consts stack-machine)
                                                      (get-names stack-machine)
                                                      (get-code stack-machine)
                                                      (get-IC stack-machine))]
    [(equal? symbol 'CO-CONSTS) (make-stack-machine (get-stack stack-machine)
                                                    (get-varnames stack-machine)
                                                    item
                                                    (get-names stack-machine)
                                                    (get-code stack-machine)
                                                    (get-IC stack-machine))]
    [(equal? symbol 'CO-NAMES) (make-stack-machine (get-stack stack-machine)
                                                   (get-varnames stack-machine)
                                                   (get-consts stack-machine)
                                                   item
                                                   (get-code stack-machine)
                                                   (get-IC stack-machine))]
    [(equal? symbol 'CO-CODE) (make-stack-machine (get-stack stack-machine)
                                                  (get-varnames stack-machine)
                                                  (get-consts stack-machine)
                                                  (get-names stack-machine)
                                                  item
                                                  (get-IC stack-machine))]
    [(equal? symbol 'INSTRUCTION-COUNTER) (make-stack-machine (get-stack stack-machine)
                                                              (get-varnames stack-machine)
                                                              (get-consts stack-machine)
                                                              (get-names stack-machine)
                                                              (get-code stack-machine)
                                                              item)]))

;; Primeste o valoare care o pune pe stiva masinii si o intoarce actualizata
(define (push-exec-stack value stack-machine)
  (make-stack-machine (push value (get-stack stack-machine))
                      (get-varnames stack-machine)
                      (get-consts stack-machine)
                      (get-names stack-machine)
                      (get-code stack-machine)
                      (get-IC stack-machine)))

;; Primeste masina stiva si face pop pe stiva acesteia si o intoarce actualizata
(define (pop-exec-stack stack-machine)
  (make-stack-machine (pop (get-stack stack-machine))
                      (get-varnames stack-machine)
                      (get-consts stack-machine)
                      (get-names stack-machine)
                      (get-code stack-machine)
                      (get-IC stack-machine)))

;; Incrementeaza IC
(define (inc-IC stack-machine)
  (update-stack-machine (+ (get-IC stack-machine) 1)
                        'INSTRUCTION-COUNTER stack-machine))

;; Intoarce codul de la IC
(define (get-code-IC stack-machine)
  (car (drop (get-code stack-machine) (get-IC stack-machine))))

;; Face pop la elementul din varful stivei
(define (pop-top stack-machine)
  (update-stack-machine (pop (get-stack stack-machine)) 'STACK stack-machine))

;; Muta pe stiva elementul, de la pozitia data din cod, din hash-ul de constante
(define (load-const stack-machine)
  (push-exec-stack
   (hash-ref (get-consts stack-machine) (cdr (get-code-IC stack-machine)))
   stack-machine))

;; Muta de pe stiva in hash-ul de varnames prima valoare si face pop la stiva
(define (store-fast stack-machine)
  (update-stack-machine (pop (get-stack stack-machine)) 'STACK
                        (update-stack-machine
                         (hash-set (get-varnames stack-machine)
                                   (cdr (get-code-IC stack-machine))
                                   (top (get-stack stack-machine)))
                         'CO-VARNAMES stack-machine)))

;; Pune pe stiva valoarea din varnames a carei pozitie e data in cod
(define (load-fast stack-machine)
  (push-exec-stack (hash-ref (get-varnames stack-machine)
                             (cdr (get-code-IC stack-machine)))
                   stack-machine))

;; Aduna primele 2 elemente de pe stiva, le da pop, si pune elementul
;; obtinut pe stiva
(define (add stack-machine)
  (let ((TOS (top (get-stack stack-machine)))
        (TOS1 (top (cdr (get-stack stack-machine))))
        (stackm (pop-exec-stack (pop-exec-stack stack-machine))))
    (push-exec-stack (+ TOS TOS1) stackm)))

;; Acelasi proces ca mai sus, dar doar ca pentru scadere
(define (subtract stack-machine)
  (let ((TOS (top (get-stack stack-machine)))
        (TOS1 (top (cdr (get-stack stack-machine))))
        (stackm (pop-exec-stack (pop-exec-stack stack-machine))))
    (push-exec-stack (- TOS1 TOS) stackm)))

;; La fel ca la adunare doar ca pentru modulo
(define (mod stack-machine)
  (let ((TOS (top (get-stack stack-machine)))
        (TOS1 (top (cdr (get-stack stack-machine))))
        (stackm (pop-exec-stack (pop-exec-stack stack-machine))))
    (push-exec-stack (modulo TOS1 TOS) stackm)))

;; Sare la jumatate din pozitia data in cod (in lista in care se salveaza codul
;; se folosesc perechi), actualizand IC-ul cu noua sa valoare. La final se mai
;; scade 1 pentru ca se va aduna la final la loc
(define (jump-absolute stack-machine)
  (update-stack-machine (- (quotient (cdr (get-code-IC stack-machine)) 2) 1)
                        'INSTRUCTION-COUNTER stack-machine))

;; Compara primele doua elemente de pe stiva, dupa ce le scoate, cu operatorul
;; din lista data de get-cmpop si pune rezultatul pe stiva
(define (compare-op stack-machine)
  (let ((TOS (top (get-stack stack-machine)))
        (TOS1 (top (cdr (get-stack stack-machine))))
        (stackm (pop-exec-stack (pop-exec-stack stack-machine))))
    (push-exec-stack ((get-cmpop (cdr (get-code-IC stack-machine)))
                      TOS1 TOS) stackm)))

;; Scoate de pe stiva primul element si verifica daca este #f, daca da atunci
;; sare asemanator ca la jump-absolute
(define (pop-jump-if-false stack-machine)
  (update-stack-machine (pop (get-stack stack-machine)) 'STACK
                        (if (not (top (get-stack stack-machine)))
                            (update-stack-machine
                             (- (quotient (cdr (get-code-IC stack-machine)) 2) 1)
                             'INSTRUCTION-COUNTER stack-machine)
                            stack-machine)))

;; La fel ca mai sus, dar pentru cazul in care este #t (nu se mai neaga conditia)
(define (pop-jump-if-true stack-machine)
  (update-stack-machine (pop (get-stack stack-machine)) 'STACK
                        (if (top (get-stack stack-machine))
                            (update-stack-machine
                             (- (quotient (cdr (get-code-IC stack-machine)) 2) 1)
                             'INSTRUCTION-COUNTER stack-machine)
                            stack-machine)))

;; Cat timp nu s-a golit iteratorul, il scoate de pe stiva, il imparte in
;; primul element si lista cu restul si le pune inapoi. Daca s-a terminat
;; iteratorul atunci sare cu valoarea data in cod
(define (for-iter stack-machine)
  (if (null? (top (get-stack stack-machine)))
      (update-stack-machine (+ (quotient (cdr (get-code-IC stack-machine)) 2)
                               (get-IC stack-machine))
                            'INSTRUCTION-COUNTER (pop-exec-stack stack-machine))
      (let func ((stackm (pop-exec-stack stack-machine))
                 (nr (car (top (get-stack stack-machine))))
                 (rest (cdr (top (get-stack stack-machine)))))
        (push-exec-stack nr (push-exec-stack rest stackm)))))

;; Pune pe stiva elementul din names de la pozitia data in cod
(define (load-global stack-machine)
  (push-exec-stack
   (hash-ref (get-names stack-machine) (cdr (get-code-IC stack-machine)))
   stack-machine))

;; Inainte de a chema functia, call-function construieste o lista cu parametrii
;; ce urmeaza sa fie data functiei si apoi apeleaza functia cand s-au terminat
;; de procesat toti parametrii
(define (call-function stack-machine)
  (let loop ((n (cdr (get-code-IC stack-machine)))
             (params '()) (stackm stack-machine))
    (if (< n 1)
        (push-exec-stack ((get-function (top (get-stack stackm))) params)
                         (pop-exec-stack stackm))
        (loop (- n 1) (cons (top (get-stack stackm)) params)
              (pop-exec-stack stackm)))))

;; Cat timp nu s-a ajuns la return, adica la finalul programului, se
;; incrementeaza instruction counter-ul si apoi in functie de ce instructiune e
;; in cod, se executa o functie, sau se sare peste daca instructiunea se doreste
;; a fi ignorata
(define (run-stack-machine stack-machine)
  (if (equal? (car (get-code-IC stack-machine)) 'RETURN_VALUE)
      stack-machine
      (run-stack-machine
       (inc-IC
        (cond 
          [(equal? (car (get-code-IC stack-machine)) 'POP_TOP)
           (pop-top stack-machine)]
                                                                        
          [(equal? (car (get-code-IC stack-machine)) 'LOAD_CONST)
           (load-const stack-machine)]
                                                                        
          [(equal? (car (get-code-IC stack-machine)) 'LOAD_FAST)
           (load-fast stack-machine)]
                                                                        
          [(equal? (car (get-code-IC stack-machine)) 'STORE_FAST)
           (store-fast stack-machine)]

          [(or (equal? (car (get-code-IC stack-machine)) 'BINARY_ADD)
               (equal? (car (get-code-IC stack-machine)) 'INPLACE_ADD))
           (add stack-machine)]
                                                                        
          [(or (equal? (car (get-code-IC stack-machine)) 'BINARY_SUBTRACT)
               (equal? (car (get-code-IC stack-machine)) 'INPLACE_SUBTRACT))
           (subtract stack-machine)]
                                                                        
          [(or (equal? (car (get-code-IC stack-machine)) 'BINARY_MODULO)
               (equal? (car (get-code-IC stack-machine)) 'INPLACE_MODULO))
           (mod stack-machine)]
                                       
          [(equal? (car (get-code-IC stack-machine)) 'JUMP_ABSOLUTE)
           (jump-absolute stack-machine)]
                                       
          [(equal? (car (get-code-IC stack-machine)) 'COMPARE_OP)
           (compare-op stack-machine)]
                                       
          [(equal? (car (get-code-IC stack-machine)) 'POP_JUMP_IF_FALSE)
           (pop-jump-if-false stack-machine)]
                                       
          [(equal? (car (get-code-IC stack-machine)) 'POP_JUMP_IF_TRUE)
           (pop-jump-if-true stack-machine)]

          [(equal? (car (get-code-IC stack-machine)) 'FOR_ITER)
           (for-iter stack-machine)]
                                       
          [(equal? (car (get-code-IC stack-machine)) 'LOAD_GLOBAL)
           (load-global stack-machine)]
                                       
          [(equal? (car (get-code-IC stack-machine)) 'CALL_FUNCTION)
           (call-function stack-machine)]
                                       
          [else stack-machine])))))
