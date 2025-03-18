#lang racket
(require 2htdp/image)

;----------------------------CONDICIONES MULTIPLES----------------------------

;EJERCICIO 1

(define (sgn2 x) (cond[(< x 0) -1]
                      [(= x 0) 0]
                      [(> x 0) 1]))

(sgn2 (- 2 3))
(sgn2 6)

;EJERCICIO 2 y 3

(define (angost? x) (cond [(< (image-width x) (image-height x)) "Angosta"]
                          [(> (image-width x) (image-height x)) "Ancha"]
                          [#t "Cuadrada"]))

(angost? (rectangle 100 1000 "solid" "blue"))

(define (equi? x y z) (cond [(and (= x y) (= y z) (= 180 (+ x y z))) "Equilatero"]
                            [(and (not (= x y)) (not (= y z)) (not (= x z)) (= 180 (+ x y z))) "Escaleno"]
                            [(not (= 180 (+ x y z))) "No es triangulo"]
                            [#t "Isosceles"]))

(equi?  90 45 45)

(define (pitagorica? x y z) (cond [(or (= x (sqrt (+ (* y y) (* z z)))) (= y (sqrt (+ (* x x) (* z z)))) (= z (sqrt (+ (* x x) (* y y))))) #t]
                                  [#t #f]))

(pitagorica? 3 5 4)

;EJERCICIO 4

(define (angosta2? x) (cond [(> (image-width x) (* 2 (image-height x))) "Muy ancha"]
                            [(> (image-height x) (* 2 (image-width x))) "Muy angosta"]
                            [#t (angost? x)]))

(angosta2? (rectangle 100 1000 "solid" "blue"))

;EJERCICIO 5

(define (clasificar t) (cond [(< t 0) "Muy frío (MF)"]
                             [(and (>= t 0) (< t 15)) "Frío (F)"]
                             [(and (>= t 15) (< t 25)) "Agradable (A)"]
                             [(>= t 25) "Caluroso (C)"]))

(clasificar 0)

;----------------------------PREDICADOS----------------------------

;EJERCICIO 6

(define (convertBool x) (cond [(and #t x) 1]
                              [#t 0]))

(define (signoBoolString x) (cond[(number? x) (sgn2 x)]
                                 [(string? x) (sgn2 (string->number x))]
                                 [(boolean? x) (sgn2 (convertBool x))]))

(signoBoolString #f)

;EJERCICIO 7

(define (convertImage x) (cond [(string=? (angost? x) "Ancha") 1]
                               [(string=? (angost? x) "Angosta") -1]
                               [#t 0]))

(define (signo4 x) (cond [(number? x) (sgn2 x)]
                         [(string? x) (sgn2 (string->number x))]
                         [(boolean? x) (sgn2 (convertBool x))]
                         [(image? x) (sgn2 (convertImage x))]))

(signo4 (rectangle 100 100 "solid" "blue"))

;EJERCICIO 8

(define (signo5 x) (cond [(number? x) (sgn2 x)]
                         [(string? x) (sgn2 (string->number x))]
                         [(boolean? x) (sgn2 (convertBool x))]
                         [(image? x) (sgn2 (convertImage x))]
                         [#t "Clase no soportada por la funcion"]))

;EJERCICIO 9

(define (signo6 x) (cond [(number? x) (sgn2 x)]
                         [(and (string? x) (= 0 (convertBool (string->number x)))) "La cadena no se puede convertir a un numero"]
                         [(string? x) (sgn2 (string->number x))]
                         [(boolean? x) (sgn2 (convertBool x))]
                         [(image? x) (sgn2 (convertImage x))]
                         [#t "Clase no soportada por la funcion"]))



