;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname triangulito) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;IDEA GENERAL DEL PROGRAMA: hacer que un triangulo se mueva de izq a der rotando en sentido horario
; y que vuelva rotando en sentido antihorario.
(require 2htdp/image)
(require 2htdp/universe)
(define-struct tri [x dir rot])

(define ANCHO 500)
(define ALTO 100)
(define ESCENA (empty-scene ANCHO ALTO))
(define tamaño 20)
(define INI (make-tri tamaño "derecha" 0))
(define DELTA 2)

(define (draw e) (place-image (rotate (tri-rot e) (triangle tamaño "solid" "blue")) (tri-x e) (/ ALTO 2) ESCENA))

(define (dir? e) (cond [(and (string=? (tri-dir e) "derecha") (= (tri-x e) (- ANCHO (/ tamaño 2)))) "izquierda"]
                       [(and (string=? (tri-dir e) "izquierda") (= (tri-x e) (/ tamaño 2))) "derecha"]
                       [#t (tri-dir e)]))


(define (time? e) (cond [(string=? (dir? (make-tri (tri-x e) (tri-dir e) (tri-rot e))) "derecha") (make-tri (+ (tri-x e) DELTA) "derecha" (- (tri-rot e) 1))]
                        [(string=? (dir? (make-tri (tri-x e) (tri-dir e) (tri-rot e))) "izquierda") (make-tri (- (tri-x e) DELTA) "izquierda" (+ (tri-rot e) 1))]))

(big-bang INI

  [to-draw draw]
  [on-tick time?]

  )










