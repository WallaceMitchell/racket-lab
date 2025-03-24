;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |practica lab 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;--------DISEÑO DE FUNCIONES SIMPLES--------

;EJERCICIO 1

;Representamos las coordenadas del punto mediante numeros
;dist: Number Number -> Number (recibe dos coordenadas x e y de un punto y devuelve la distancia al origen)
;input: 1 0, output: 1
;input: 1 1, output: sqrt 2
;input: 0 2, output: 2

(define (dist x y) (sqrt (+ (* x x) (* y y))))
(dist 1 0)
(dist 1 1)
(dist 0 2);resultados esperados coinciden con obtenidos

;EJERCICIO 2

;Representamos las coordenadas y distancia mediante numeros
;distToPoint: Number Number Number Number -> Number (toma las coordenadas de 2 puntos y devuelve la distancia que hay entre ellos)
;input: 0 0 1 0, output: 1
;input: 0 0 0 2, output: 2
;input: 1 1 1 2, output: 1


(define (distToPoint x y a b) (sqrt (+ (expt (abs (- x a)) 2) (expt (abs (- y b)) 2))))
(distToPoint 0 0 1 0)
(distToPoint 0 0 0 2)
(distToPoint 1 1 1 2);resultados esperados coinciden con obtenidos

;EJERCICIO 3

;Representamos la medida de la arista con un numero
;vol-cubo: Number -> Number (toma la medida de la arista y devuelve su volumen)
;input: 1, output: 1
;input: 2, output 8
;input 4, output 64

(define (vol-cubo a) (* a a a))
(vol-cubo 1)
(vol-cubo 2)
(vol-cubo 4);resultados esperados coinciden con obtenidos

;EJERCICIO 4

;Representamos la medida de la arista con un numero
;area-cubo: Number -> Number (toma la medida de la arista de un cubo y devuelve el area total del mismo)
;input: 1, output: 6
;input: 2, output: 24
;input: 5, output: 150

(define (area-cubo a) (* 6 a a))
(area-cubo 1)
(area-cubo 2)
(area-cubo 5);resultados esperados coinciden con obtenidos

;EJERCICIO 5

;Representamos la cadena dada y la modificada con el tipo string, el indice lo representamos con un numero
;string-insert: String Number -> String (toma una cadena y la devuelve con el - agregado en la posicion dada)
;input: "hola" 0, output: "-hola"
;input "hola" 3, output: "hol-a"
;input "argentina" 5, output: "argen-tina"

(define (string-insert s i) (string-append  (substring s 0 i) "-" (substring s i (string-length s))))
(string-insert "hola" 0)
(string-insert "hola" 3)
(string-insert "argentina" 5);resultados esperados coinciden con obtenidos

;EJERCICIO 6

;Representamos la cadena como un dato string
;string-last: String -> String (toma una cadena y devuelve su ultimo caracter)
;input: "hola", output: "a"
;input: "chau", output: "u"
;input: "hasta luego", output "o"


(define (string-last s) (substring s (- (string-length s) 1) (string-length s)))
(string-last "hola")
(string-last "chau")
(string-last "hasta luego");resultados esperados coinciden con obtenidos

;EJERCICIO 7 y 8 (solo pruebo el check-expect con la funcion del ej 7)

;Representamos la cadena como un dato string
;string-remove-last: String -> String (toma una cadena y devuelve la misma cadena sin el ultimo caracter)
;input: "hola", output: "hol"
;input: "argentina", output: "argentin"
;input: "chau", output: "cha"

;(check-expect (string-remove-last "hola") "hol")
;(check-expect (string-remove-last "argentina") "argentin")
;(check-expect (string-remove-last "chau") "cha")

(define (string-remove-last s) (substring s 0 (- (string-length s) 1)))
(string-remove-last "hola")
(string-remove-last "argentina")
(string-remove-last "chau");resultados esperados coinciden con obtenidos

;EJERCICIO 9


(check-expect (monto-persona 1 5) 2437.5) ;probamos solo con Jose

(define cuota 650)
(define (monto-persona p m) (cond [(and (>= p 3) (>= m 3)) (* m (- cuota (* cuota (/ 35 100))))]
                                  [(and (= m 2) (>= p 3)) (* 2 (- cuota (* cuota (/ 35 100))))]
                                  [(and (= p 2) (>= m 3)) (* m (- cuota (* cuota (/ 35 100))))]
                                  [(and (= p 2) (= m 2)) (* 2 (- cuota (* cuota (/ 25 100))))]
                                  [(>= p 3) (- cuota (* cuota (/ 20 100)))]
                                  [(>= m 3) (* m (- cuota (* cuota (/ 25 100))))]
                                  [(= p 2) (- cuota (* cuota (/ 10 100)))]
                                  [(= m 2) (* 2 (- cuota (* cuota (/ 15 100))))]
                                  [#true cuota]))

;EJERCICIO 10


(check-expect (anemia 125 12) #t)
(define (anemia e h) (cond [(and (<= e 1) (< h 13)) #t]
                           [(<= e 1) #f]
                           [(and (> e 1) (<= e 6) (< h 10)) #t]
                           [(and (> e 1) (<= e 6)) #f]
                           [(and (> e 6) (<= e 12) (< h 11)) #t]
                           [(and (> e 6) (<= e 12)) #f]
                           [(and (> e 12) (<= e 60) (< h 11.5)) #t]
                           [(and (> e 12) (<= e 60)) #f]
                           [(and (> e 60) (<= e 120) (< h 12.6)) #t]
                           [(and (> e 60) (<= e 120)) #f]
                           [(and (> e 120) (< h 13)) #t]
                           [#t #f]))

;EJERCICIO 11

(check-expect (autopromedio 3 1 6) 10)
(define (canProm? a b c) (or (= a (/ (+ b c) 2)) (= b (/ (+ a c) 2)) (= c (/ (+ a b) 2)))) ;devolvera un #t en caso de que sea autopromediable, sino #f
(define (autopromedio a b c) (cond [(boolean=? #t (canProm? a b c)) (* a b c)]
                                   [#t (+ a b c)]))

;EJERCICIO 12

(define ruta 11)
(define ciudad 8)
(define grado2 1)
(define grado3 1.1)
(define (ciudadRuta x y) (string-append "Autonomia en ciudad: " (number->string x) "km. Autonomia en ruta: " (number->string y) "km."))
(define (autonomia l g) (cond [(= g grado2) (ciudadRuta (* l ciudad) (* l ruta))]
                              [#t (ciudadRuta (* l ciudad grado3) (* l ruta grado3))]))

(autonomia 20 grado3)


