#lang racket

(provide get-opname)
(provide get-cmpop)
(provide get-function)

(define opcodes (hash
  1 'POP_TOP
  22 'BINARY_MODULO
  23 'BINARY_ADD
  24 'BINARY_SUBTRACT
  55 'INPLACE_ADD
  56 'INPLACE_SUBTRACT
  59 'INPLACE_MODULO
  68 'GET_ITER
  83 'RETURN_VALUE
  87 'POP_BLOCK
  93 'FOR_ITER
  100 'LOAD_CONST
  107 'COMPARE_OP
  113 'JUMP_ABSOLUTE
  114 'POP_JUMP_IF_FALSE
  116 'LOAD_GLOBAL
  120 'SETUP_LOOP
  124 'LOAD_FAST
  125 'STORE_FAST
  131 'CALL_FUNCTION
  ))

;; Functions reprezinta un hash cheie-valoare, care returneaza o functie in
;; functie de cheia dorita:
;;   - print -> afiseaza la consola mesalul dorit
;;   - range -> construieste o lista cu elementele de la 0 la n-1
;;   - sqrt  -> apeleaza functia sqrt din racket
;;   - prod  -> inmulteste toate elementele din lista de parametri
(define functions (hash "print" (lambda (msg) (writeln (car msg)))
                        "range" (lambda (n)
                                  (let loop ((max (- (car n) 1)) (L '()))
                                    (if (= max 0)
                                        (cons max L)
                                        (loop (- max 1) (cons max L)))))
                        "sqrt" (lambda (x) (sqrt (car x)))
                        "prod" (lambda (L) (foldr * 1 L))))

(define cmpcodes (list < <= eq? (compose not eq?) > >= member (compose not member)))

(define get-opname ((curry hash-ref) opcodes))
(define get-cmpop ((curry list-ref) cmpcodes))
(define get-function ((curry hash-ref) functions))
