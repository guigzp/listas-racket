#lang racket

(define lista (list 1 2 3 4 5 6 10))

(define (par? x)
  (if(= (remainder x 2) 0) #t #f))

(struct arvore-bin (v esq dir) #:transparent)

(define t0 (arvore-bin 10 empty empty))

(define t1 (arvore-bin 9
                       t0
                       empty))

(define t2 (arvore-bin 7
                       (arvore-bin 8 empty empty)
                       t1))

(define t3 (arvore-bin 4
                       (arvore-bin 3 empty empty)
                       empty))

(define t4 (arvore-bin 3
                       t2
                       t2))

; Exercício 1
; Numero, Lista -> Booleano
; Função que recebe um número e uma lista e devolve se o número está na lista
(define (acha x lst)
  (cond [(empty? lst) #f]
        [(= (first lst) x) #t]
        [else (acha x (rest lst))]))

; Exercício 2
; Numero, Lista -> Lista
; Função que recebe um número e uma lista, retornando a lista sem o número
(define (removeall x lst)
  (cond [(empty? lst) empty]
        [else
         (cond
           [(equal? (first lst) x) (removeall x (rest lst))]
           [else
            (cons (first lst) (removeall x (rest lst)))
            ])]))

; Exercício 3
; Numero, Lista -> Lista
; Função que recebe um número e uma lista, devolvendo a lista original com o número adicionado no final da lista
(define (addend x lst)
  (cond [(empty? lst) (list x)]
        [(empty? (rest lst)) (list (first lst) x)]
        [else
         (cons (first lst) (addend x (rest lst)))]))

; Exercício 4
; Lista -> Lista
; Função que recebe uma lista e retorna a mesma com os elementos invertidos
(define (inverte lst lst2)
  (cond [(empty? lst) lst2]
        [else
         (addend (first lst) lst2) (inverte (rest lst) lst2)]))

; Exercício 6
; Numero, Lista -> Lista
; Função recebe uma lista e um número, devolvendo a lista com os elementos somados ao número
(define (somanumero x lst)
  (cond [(empty? lst) empty]
        [else
         (cons (+(first lst)x) (somanumero x (rest lst)))]))

; Exercício 7
; Lista -> Lista
; Função que recebe uma lista e devolve a lista sem números pare
(define (removepair lst)
  (cond [(empty? lst) empty]
        [(par? (first lst)) (removepair(rest lst))]
        [(cons (first lst) (removepair (rest lst)))]))

; Exercício 8
; Lista -> Numero
; Função que recebe uma lista e devolve o último termo
(define (ultimotermo lst)
  (cond [(empty? lst) (error "A lista está vazia")]
        [(empty? (rest lst)) (first lst)]
        [(ultimotermo (rest lst))]))

; Exercício 10
; Numero, Lista -> Lista
; Função que recebe um número e uma lista ordenada em ordem crescente, insere o número na lista em ordem crescente
(define (inserecrescente x lst)
  (cond [(empty? lst) (list x)]
        [(> x (first lst)) (cons (first lst) (inserecrescente x (rest lst)))]
        [(cons x (cons (first lst) (rest lst)))]))

; Exercício 15
; Numero, Arvore Binária -> Arvore Binária
; Função que recebe um número e uma árvore binária, devolvendo a árvore com os elementos somados ao número
(define (sarvore x tree)
  (cond [(empty? tree) empty]
        [(arvore-bin (+ x (arvore-bin-v tree)) (sarvore x (arvore-bin-esq tree)) (sarvore x (arvore-bin-dir tree)))]))

; Exercício 16
; Arvore Binaria -> Booleano
; Função que recebe uma Arvore Binaria e devolve se ela é uma Arvore Binaria de Busca
(define (ehdebusca tree)
  (cond [(empty? (arvore-bin-v tree)) #t]
        [(and (empty? (arvore-bin-esq tree)) (empty? (arvore-bin-dir tree))) #t]
        [(empty? (arvore-bin-esq tree))
         (cond [(and (> (arvore-bin-v tree) (arvore-bin-v (arvore-bin-dir tree))) (ehdebusca (arvore-bin-dir tree))) #t]
               [else #f])]
        [(empty? (arvore-bin-dir tree))
         (cond [(and (> (arvore-bin-v tree) (arvore-bin-v (arvore-bin-esq tree))) (ehdebusca (arvore-bin-esq tree))) #t]
               [else #f])]
        [else (cond [(and (> (arvore-bin-v tree) (arvore-bin-v (arvore-bin-dir tree))) (> (arvore-bin-v tree) (arvore-bin-v (arvore-bin-esq tree)))
                     (ehdebusca (arvore-bin-esq tree)) (ehdebusca (arvore-bin-esq tree))) #t]
               [else #f])]))

; Exercício 17
; Numero, Arvore Binária -> Booleano
; Função que recebe um número e uma árvore binária de busca, devolvendo se o número está ou não na árvore
(define (buscabin x tree)
  (cond [(empty? tree) #f]
        [(= x (arvore-bin-v tree)) #t]
        [(< x (arvore-bin-v tree)) (buscabin x (arvore-bin-esq tree))]
        [(> x (arvore-bin-v tree)) (buscabin x (arvore-bin-dir tree))]))