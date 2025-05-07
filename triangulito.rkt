;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname triangulito) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;IDEA GENERAL DEL PROGRAMA: hacer que un triangulo se mueva de izq a der rotando en sentido horario
; y que vuelva rotando en sentido antihorario.
(require 2htdp/image)
(require 2htdp/universe)

;estructura que representa un triangulo en movimiento
;x: Number (posicion en el eje x del triangulo)
;dir: String (direccion en la que se esta moviendo el triangulo)
;rot : Number (grados de rotacion del triangulo)
(define-struct tri [x dir rot])

(define ANCHO 500)
(define ALTO 100)
(define ESCENA (empty-scene ANCHO ALTO))
(define tamaño 20)

;Estado inicial del big-bang (estructura tipo tri). Mi Estado será una estructura tri
(define INI (make-tri tamaño "derecha" 0))
(define DELTA 2)

;graficador del big-bang
;draw: Estado -> Image (toma un estado y devuelve una imagen con el triangulo en la posicion y rotacion adecuados)
(define (draw e) (place-image (rotate (tri-rot e) (triangle tamaño "solid" "blue")) (tri-x e) (/ ALTO 2) ESCENA))

;funcion auxiliar que me ayuda a determinar la direccion del estado tri que le paso como entrada
;(si llega al final de la escena o al inicio, debe cambiar, sino se mantiene)
;dir?: Estado -> String
(define (dir? e) (cond [(and (string=? (tri-dir e) "derecha") (= (tri-x e) (- ANCHO (/ tamaño 2)))) "izquierda"]
                       [(and (string=? (tri-dir e) "izquierda") (= (tri-x e) (/ tamaño 2))) "derecha"]
                       [#t (tri-dir e)]))

;controlador de ticks de reloj del big-bang que cambia la posicion y rotacion en base a la direccion especificada mediante dir?
;time?: Estado -> Estado
(define (time? e) (cond [(string=? (dir? (make-tri (tri-x e) (tri-dir e) (tri-rot e))) "derecha") (make-tri (+ (tri-x e) DELTA) "derecha" (- (tri-rot e) 1))]
                        [(string=? (dir? (make-tri (tri-x e) (tri-dir e) (tri-rot e))) "izquierda") (make-tri (- (tri-x e) DELTA) "izquierda" (+ (tri-rot e) 1))]))

;big-bang
(big-bang INI

  [to-draw draw]
  [on-tick time?]

  )

;chequeamos ejemplos de draw
(check-expect (draw INI) (place-image (rotate 0 (triangle tamaño "solid" "blue")) tamaño (/ ALTO 2) ESCENA))
(check-expect (draw (make-tri 2 "derecha" -2)) (place-image (rotate -2 (triangle tamaño "solid" "blue")) 2 (/ ALTO 2) ESCENA))
(check-expect (draw (make-tri 70 "derecha" -70)) (place-image (rotate -70 (triangle tamaño "solid" "blue")) 70 (/ ALTO 2) ESCENA))

;chequeamos ejemplos de dir?
(check-expect (dir? INI) "derecha")
(check-expect (dir? (make-tri 494 "izquierda" 6)) "izquierda")
(check-expect (dir? (make-tri 4 "derecha" -4)) "derecha")
;chequeamos ejemplos de time?
(check-expect (time? INI) (make-tri (+ tamaño 2) "derecha" -1))
(check-expect (time? (make-tri 2 "derecha" -1)) (make-tri 4 "derecha" -2))







