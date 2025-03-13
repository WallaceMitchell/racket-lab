#lang racket
(require 2htdp/image)

;-------------------ARITMETICA-------------------

;EJERCICIO 1


;a.1

(-(* 12 5)(* 7 6))

;a.2

(+ (- (* 5 3) (/ (* 7 4) 14)) (/ 3 1))

;a.3

(+ (cos 0.8)(+ (/ 1 5) (* (sin 0.5) 3.5)))

;EJERCICIO 2

;a

(/ 1 (sin (sqrt 3)))

;b

(* (sqrt 2) (sin pi))

;c

(+ 3 (sqrt (- 4)))

;d

(* (sqrt 5) (sqrt (/ 3 (cos pi))))

;e

;(/ (sqrt 5) (sin (* 3 0))) ---> expresion incapaz de evaluacion, error es dividir por 0

;f

(/ (+ 3) (* 2 4))

;g

(* 1 2 3 4 5 6 7 8)

;h

(/ 120 2 3 2 2 5)

;EJERCICIO 3

(floor (min 10 7.67 6.3))

;-------------------STRINGS-------------------

;1

;a

(string-append "Hola " "Mundo")

;b

(string-append "Pro" "gra" "ma")

;c

(number->string 1357)

;d y e

(string-append "La respuesta es " (number->string (+ 21 21)))

;f

(* (string-length "Hola") (string-length "Chau"))

(substring "Programar" 0 9)

;-------------------BOOLEANOS Y NUMEROS-------------------

;1

;a

(not #t)

;b

(or #t #f)

;c

(and #t #f)

;d

(and #t (or #f (not #f)) (not #t))

;e

(not (= 2 (* 1 3)))

;f

(or (= 2 (* 1 3)) (< 4 (+ 3 2)))

;EJERCICIO 2

;a

(> (cos 0) 0)

;b

(= 14 (string-length "Hola, Mundo"))

;c

(and (> pi 3) (< pi 4))

;d

(= (* 5 5) (sqrt 625))

;e

(not (string=? "a" (substring "Ada Lovelace" 2 3)))

;-------------------IMAGENES-------------------

;EJERCICIO 1

;a

(circle 100 "solid" "red")

;b

(rectangle 200 100 "outline" "green")

;c

 (rectangle 200 100 "solid" "silver")

;d

 (overlay (rectangle 20 20 "solid" "blue") (circle 40 "solid" "green"))

;e

(empty-scene 150 125)

;f

(place-image (circle 20 "solid" "red") 50 50 (empty-scene 100 100))

;g

(+ (image-width (circle 10 "solid" "red")) (image-height (rectangle 10 20 "solid" "blue")))

;-------------------FUNCIONES-------------------


(define (f x) (+ 1 x))
(define (doble x) (* x 2))
(define (cuad-azul x) (square x "solid" "blue"))
(define (h x y) (< x (doble y)))
(define (orig x y) (sqrt (+ (* x x) (* y y))))
(define (dist a b x y) (sqrt (+ (* (abs (- a x)) (abs (- a x))) (* (abs (- b y)) (abs (- b y))))))
(define (vol-cube y) (* y y y))
(define (area-cube x) (* 6 (* x x)))
(define (metro-pie x) (* 3.28084 x))

;EJERCICIO 1

;a

(cuad-azul (doble 10))

;b

(and (h 2 3) (h 3 4))

;c

(= (f 1) (doble 1))

;EJERCICIO 2

(orig 3 4)

;EJERCICIO 3

(dist 0 0 2 2)

;EJERCICIO 4

(vol-cube 3)

;EJERCICIO 5

(area-cube 4)

;EJERCICIO 6

(metro-pie 3)

;EJERCICIO 7

