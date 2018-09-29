#lang racket

; Exercício 2
; Lista, Número -> Lista
; Devolve os n primeiros números de uma lista
(define (nPrimeiros lst n)
  (cond [(> n (length lst)) #f]
        [(zero? n) empty]
        [else (cons (first lst) (nPrimeiros (rest lst) (sub1 n)))]))

; Exercício 3
; Lista, Número -> Lista
; Retira os n primeiros números de uma lista
(define (dropPrimeiros lst n)
  (cond [(> n (length lst)) empty]
        [(> n 0) (dropPrimeiros (rest lst) (sub1 n))]
        [else lst]))

; Exercício 4
; Lista, Número -> Lista
; Remove o elemento da posição passada da lista
(define (remove-n lst n)
  (cond [(> n (length lst)) #f]
        [(zero? (sub1 n)) (rest lst)]
        [else (cons (first lst) (remove-n (rest lst) (sub1 n)))]))

; Exercício 5
; Lista, Número, Número -> Lista
; Insere um número em uma posição da lista


; Exercício 8
; Lista, Lista -> Lista
; Concatena duas listas
(define (concat-list lst1 lst2)
  (cond [(empty? lst1) lst2]
        [else (cons (first lst1) (concat-list (rest lst1) lst2))]))


; Exercício 9
; Lista, Lista -> Lista
; Recebe 2 listas em crescente e devolve uma nova lista com os elementos das duas listas em ordem crescente
(define (merge-list lst1 lst2)
  (cond [(and (empty? lst1) (empty? lst2)) empty]
        [(empty? lst1) lst2]
        [(empty? lst2) lst1]
        [(< (first lst1) (first lst2)) (cons (first lst1) (merge-list (rest lst1) lst2))]
        [else (cons (first lst2) (merge-list lst1 (rest lst2)))]))