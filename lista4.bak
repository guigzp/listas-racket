#lang racket

; Exercício 1
; Número -> Número
; Calcula o fatorinal de um número natural
(define (fat n)
  (if (= n 0) 1 (* n (fat (sub1 n)))))

; Exercício 2a
; Número, Número -> Número
; Calcula a soma de 2 números naturais
(define (soma x y)
  (cond [(zero? y) x] 
        [else (soma (add1 x) (sub1 y))]))

; Exercício 2b
; Número, Número -> Número
; Calcula a subtração de 2 números naturais
(define (sub x y)
    (cond [(zero? y) x] 
        [else (sub (sub1 x) (sub1 y))]))

; Exercício 2c
; Número, Número -> Número
; Calcula a multiplicação de 2 números naturais

;Exercício 3
; Número -> Booleano
; Duas funções, par e impar que usam recursão indireta 
(define (par? x)
  (cond [(zero? x) #t]
        [else (if (impar? (sub1 x)) #t #f)]))

(define (impar? x)
  (cond [(zero? x) #f]
        [else (if (par? (sub1 x)) #t #f)]))


; Exercício 4
; Número -> Boolean
; Verifica se um número natural é perfeito
(define (perfeito x)
  (define divisores (for/list ([n (in-range 1 x)]
                       #:when (zero? (remainder x n) )) n))
  (define (soma_lista lst)
    (cond [(empty? lst) 0]
          [else (+ (first lst) (soma_lista (rest lst)))]))
  (if (= x (soma_lista divisores)) #t #f))

; Exercício 5
; Número, Número -> Número
; Conta quantos números primos existem no intervalo passado
(define (conta_primos x y)
  (define (primo? n acc)
    (cond [(= n 2) #t]
          [(= n acc) #f]
          [(zero? (remainder n acc)) #t]
          [else (primo? n (add1 acc))]))
  (define intervalo (for/list ([n (in-range x y)] )n))
  (define (verifica_primo lst)
    (cond [(empty? lst) 0]
          [(false? (primo? (first lst) 2)) (verifica_primo (rest lst))]
          [else (+ 1 (verifica_primo (rest lst)))]))
  (verifica_primo intervalo))



  
  