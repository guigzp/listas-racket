#lang racket

;; Exercício 6
;; 2 Números -> 1 Número
;; Função que recebe 2 números e devolve o maior
(define (max x y)
  (cond [(<= x y) y] (else x)))

;; Exercício 7
;; 3 Números -> 1 Número
;; Função que recebe 3 números e devolve o 
(define (quadrados x y z)
  (cond [(and (<= z x) (<= z y)) (+ (* x x) (* y y))]
        [(and (<= y x) (<= y z)) (+ (* x x) (* z z))]
        (else (+ (* y y) (* z z)))))

;; Exercício 8
;; 2 Números -> 1 Número
;; Função que recebe 2 números e devolve a distancia a origem
(define (distancia x y)
  (sqrt (+ (* x x) (* y y))))

;; Exercício 9
;; 3 Números -> String
;; Função para receber 3 lados de um triangulo e verificar
;; se o mesmoo é Equilátero, Isósceles ou Escaleno
(define (triangulo x y z)
  (cond [(and (= x y) (= x z)) "Equilátero"]
        [(and (not (= x y)) (not (= x z)) (not (= y z))) "Escaleno"]
        (else "Isósceles")
        )
  )

;; Exercício 10
;; 2 Números -> 1 Número
;; Função que recebe o peso e altura e devolve o imc
(define (imc altura peso)
  (define x (/ peso (* 2 altura)))
  (cond [(< x 18.5) "Desnutrição"]
        [(< x 24.9) "Eutrofia"]
        [(< x 29.9) "Sobrepeso"]
        [(< x 34.9) "Obesidade"]
        [(< x 39.9) "Obesidade Severa"]
        (else "Obesidade Mórbida")))
