#lang racket
;----------------------MAS FUNCIONES----------------------

(require 2htdp/image)

;EJERCICIO 1

(define (angosta? x) (if (<= (image-width x) (image-height x)) "Angosta" "Ancha"))

(angosta? (rectangle 100 50 "outline" "red"))

;EJERCICIO 2

(define (angost? x) (if (< (image-width x) (image-height x)) "Angosta" (if (= (image-width x) (image-height x)) "Cuadrada" "Ancha")))

(angost? (rectangle 100 1000 "solid" "blue"))

;EJERCICIO 3

(define (equi? x y z) (if (= x y z) "Equilatero" (if (and (not (= x y)) (not (= y z)) (not (= x z))) "Escaleno" "Isosceles"))) ;faltaria comprobar si suman 180, pero anda

(equi? 60 60 60)

;EJERCICIO 4

(define (equi2? x y z) (if (= 180 (+ x y z)) (if (= x y z) "Equilatero" (if (and (not (= x y)) (not (= y z)) (not (= x z))) "Escaleno" "Isosceles")) "No es triangulo")) ;ahora si

(equi2?  40 50 90)

;EJERCICIO 5

(define PC 60)
(define PL 8)

(define (monto c l) (cond [(and (>= c 5) (>= l 4)) (+ (* c (- PC (* PC (/ 15 100)))) (* l (- PL(* PL (/ 10 100)))))]
                          [(>= c 5) (+ (* PL l) (* c (- PC (* PC (/ 15 100)))))]
                          [(>= l 4) (+ (* c PC) (* l (- PL (* PL (/ 10 100)))))]
                          [#true (+ (* c PC) (* l PL))]))

(monto 3 6)

;EJERCICIO 6

(define (monto2 c l) (cond [(>= (+ c l) 10) (+ (* c (- PC (* PC (/ 18 100)))) (* l (- PL (* PL (/ 18 100)))))]))

(monto2 4 8)

;se entiende que es el 18% del total sin descuentos adicionales

;EJERCICIO 7

(define (pitagorica? x y z) (if (= x (sqrt (+ (* y y) (* z z)))) #true (if (= y (sqrt (+ (* x x) (* z z)))) #true (if (= z (sqrt (+ (* y y) (* x x)))) #true #false))))

(pitagorica? 3 4 5)

;EJERCICIO 8

(define (rta x y z) (string-append "Los numeros " (number->string x) ", " (number->string y) " y " (number->string z) " forman una terna pitagorica"))

(define (pitagorica2? x y z) (if (= x (sqrt (+ (* y y) (* z z))))
                                 (rta x y z) (if (= y (sqrt (+ (* x x) (* z z))))
                                           (rta x y z) (if (= z (sqrt (+ (* y y) (* x x))))
                                                     (rta x y z) "Los numeros dados no forman una terna pitagorica"))))

(pitagorica2? 3 4 6)

;EJERCICIO 9

(define (collatz n) (cond [(= 0 (modulo n 2)) (/ n 2)]
                          [#true (+ 1 (* 3 n))]))

(collatz 7)

;EJERCICIO BANDERAS

;a

(define peru (place-image (rectangle 30 60 "solid" "red")
                             15 30
                             (place-image (rectangle 30 60 "solid" "red")
                                          75 30
                                          (place-image (rectangle 30 60 "solid" "white") 45 30
                                                     (empty-scene 90 60)))))

peru

;b

(define italia (place-image (rectangle 30 60 "solid" "green")
                            15 30
                            (place-image (rectangle 30 60 "solid" "red")
                                         75 30
                                         (place-image (rectangle 30 60 "solid" "white") 45 30
                                                      (empty-scene 90 60)))))

italia

;c

(define (3bands x y z) (place-image (rectangle 30 60 "solid" x)
                                    15 30
                                    (place-image (rectangle 30 60 "solid" y) 45 30
                                                 (place-image (rectangle 30 60 "solid" z) 75 30
                                                              (empty-scene 90 60)))))

(3bands "blue" "white" "red") ;prueba con la bandera de francia (2do jajaj)

;d

(define alemania (place-image (rectangle 90 20 "solid" "black")
                              45 10
                              (place-image (rectangle 90 20 "solid" "red")
                                           45 30
                                           (place-image (rectangle 90 20 "solid" "yellow")
                                                        45 50
                                                        (empty-scene 90 60)))))

alemania

;e

(define holanda (place-image (rectangle 90 20 "solid" "red")
                             45 10
                             (place-image (rectangle 90 20 "solid" "white")
                                          45 30
                                          (place-image (rectangle 90 20 "solid" "blue")
                                                       45 50
                                                       (empty-scene 90 60)))))

holanda

;f

(define (3bandsTwo x y z) (place-image (rectangle 90 20 "solid" x)
                                       45 10
                                       (place-image (rectangle 90 20 "solid" y)
                                                    45 30
                                                    (place-image (rectangle 90 20 "solid" z)
                                                                 45 50
                                                                 (empty-scene 90 60)))))

(3bandsTwo "black" "red" "yellow") ;alemania de vuelta

;g

(define (bandFlag x y z r) (if (string=? "vertical" r) (3bands x y z) (3bandsTwo x y z)))

(bandFlag "red" "white" "blue" "horizontal") ;holanda de vuelta
(bandFlag "green" "white" "red" "vertical") ;italia de vuelta

;h

(bandFlag "blue" "white" "red" "vertical")

;i

(define sudan (place-image (rotate 270 (triangle 75 "solid" "green")) 18 30 (bandFlag "red" "white" "black" "horizontal")))

sudan

(define argentina (place-image (circle 8 "solid" "yellow") 45 30 (bandFlag "blue" "white" "blue" "horizontal")))

argentina

(define camerun (place-image (star 10 "solid" "yellow") 45 30 (bandFlag "green" "red" "yellow" "vertical")))

camerun

;j

(define brasil (place-image (circle 14 "solid" "blue")
                            45 30
                            (place-image (rotate 270 (triangle 45 "solid" "yellow"))
                            64 30
                            (place-image (rotate 90 (triangle 45 "solid" "yellow"))
                            26 30
                            (place-image (rectangle 90 60 "solid" "green")
                            45 30
                            (empty-scene 90 60))))))

brasil

;k

(define ancho 180)
(define alto 120)

(define (3bandsTest x y z) (place-image (rectangle (/ ancho 3) alto "solid" x)
                                    (/ ancho 6) (/ alto 2)
                                    (place-image (rectangle (/ ancho 3) alto "solid" y)
                                                 (/ ancho 2) (/ alto 2)
                                                 (place-image (rectangle (/ ancho 3) alto "solid" z) ;reemplazamos coordenadas y medidas usando las ctes
                                                              (* ancho (/ 5 6)) (/ alto 2)           ;de ancho y alto de la escena. Se puede hacer en
                                                              (empty-scene ancho alto)))))           ;cualquier funcion. Ejemplo con 3bands
                                                                                                     ;anteriormente definida

(3bandsTest "green" "white" "red") ;italia



