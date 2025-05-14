;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |practica 5 parte 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;EMPEZAMOS DESDE EL EJERCICIO 5

;5)

(define (contiene? s l) (cond [(empty? l) #f]
                              [(string=? (first l) s) #t]
                              [#t (contiene? s (rest l))]))

;6) eval paso a paso

;7)

(define (suma l) (cond [(empty? l) 0]
                       [#t (+ (first l) (suma (rest l)))]))

;8)

(define (pos? l) (cond [(empty? l) #t]
                       [(<= (first l) 0) #f]
                       [#t (pos? (rest l))]))

;9)

(define (todos-verdaderos l) (cond [(empty? l) #t]
                                   [(boolean=? #f (first l)) #f]
                                   [#t (todos-verdaderos (rest l))]))

(define (uno-verdadero l) (cond [(empty? l) #f]
                                [(boolean=? #t (first l)) #t]
                                [#t (uno-verdadero (rest l))]))

;10)

(define (cant-elementos l) (cond [(empty? l) 0]
                                 [#t (+ 1 (cant-elementos (rest l)))]))

;11)

(define (promedio l) (/ (suma l) (cant-elementos l)))

;12)

(define (pares l) (cond [(empty? l) l]
                        [(even? (first l)) (cons (first l) (pares (rest l)))]
                        [#t (pares (rest l))]))

;13)

(define (cortas l) (cond [(empty? l) l]
                         [(< (string-length (first l)) 5) (cons (first l) (cortas (rest l)))]
                         [#t (cortas (rest l))]))

;14)

(define (mayores l n) (cond [(empty? l) l]
                            [(> (first l) n) (cons (first l) (mayores (rest l) n))]
                            [#t (mayores (rest l) n)]))

;15)

(define MAX 5)

(define (distancia p) (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

(define (cerca l) (cond [(empty? l) l]
                        [(< (distancia (first l)) 5) (cons (first l) (cerca (rest l)))]
                        [#t (cerca (rest l))]))

;16)

(define (positivos l) (cond [(empty? l) l]
                            [(> (first l) 0) (cons (first l) (positivos (rest l)))]
                            [#t (positivos (rest l))]))

;17)

(define (eliminar l n) (cond [(empty? l) l]
                             [(= n (first l)) (eliminar (rest l) n)]
                             [#t (cons (first l) (eliminar (rest l) n))]))

;18)

(define (raices l) (cond [(empty? l) l]
                         [#t (cons (sqrt (first l)) (raices (rest l)))]))

;19)

(define (distancias l) (cond [(empty? l) l]
                             [#t (cons (distancia (first l)) (distancias (rest l)))]))

;20)

(define (anchos l) (cond [(empty? l) l]
                         [#t (cons (image-width (first l)) (anchos (rest l)))]))

;21)

(define (signo x) (cond [(< x 0) -1]
                        [(= x 0) 0]
                        [#t 1]))

(define (signos l) (cond [(empty? l) l]
                         [#t (cons (signo (first l)) (signos (rest l)))]))

;22)

(define (cuadrados l) (cond [(empty? l) l]
                            [#t (cons (sqr (first l)) (cuadrados (rest l)))]))

;23)

(define (longitudes l) (cond [(empty? l) l]
                             [#t (cons (string-length (first l)) (longitudes (rest l)))]))

;24)

(define (convertirFC l) (cond [(empty? l) l]
                              [#t (cons (* (- (first l) 32) 5/9) (convertirFC (rest l)))]))

;25)

(define (prod l) (cond [(empty? l) 1]
                       [#t (* (first l) (prod (rest l)))]))

;26)

(define (pegar l) (cond [(empty? l) ""]
                        [#t (string-append (first l) (pegar (rest l)))]))

;27)

(define (maximo l) (cond [(empty? l) 0]
                         [(= 1 (length l)) (first l)]
                         [(>= (first l) (first (rest l))) (maximo (cons (first l) (rest (rest l))))]
                         [#t (maximo (cons (first (rest l)) (rest (rest l))))]))

;28)

(define (sumdist l) (cond [(empty? l) 0]
                          [#t (+ (distancia (first l)) (sumdist (rest l)))]))

;29)

(define (sumcuad l) (cond [(empty? l) 0]
                          [#t (+ (sqr (first l)) (sumcuad (rest l)))]))

;30)

(define ANCHO 500)
(define ALTO 500)
(define ESCENA (empty-scene ANCHO ALTO))
#|
(define INI empty)
(define ESTRELLA (star 10 "solid" "black"))

(define (draw e) (cond [(empty? e) ESCENA]
                       [#t (place-image ESTRELLA (posn-x (first e)) (posn-y (first e)) (draw (rest e)))]))

(define (mouse n x y e) (cond [(string=? e "button-down") (cons (make-posn x y) n)]
                              [#t n]))

(define (keyboard e k) (cond [(key=? k " ") INI]
                             [(key=? k "\b") (rest e)]
                             [#t e]))



  
(big-bang INI

  [to-draw draw]
  [on-mouse mouse]
  [on-key keyboard]


  )

|#

;31)

(define-struct Turno [ant act])
(define ESTADO-INICIAL (make-Turno empty empty))

(define (draw e) (cond [(even? (length (Turno-ant e)))
                        (place-image/align (text "Turno: Player 1" 20 "red") (/ ANCHO 2) (/ ALTO 2) "center" "center" ESCENA)]
                       [#t (place-image/align (text "Turno: Player 2" 20 "blue") (/ ANCHO 2) (/ ALTO 2) "center" "center" ESCENA)]))

(define (agregarUltimo l x) (cond [(empty? l) (cons x empty)]
                                  [#t (cons (first l) (agregarUltimo (rest l) x))]))

(define (fin? e) (if (comparar (cons "fin" empty) (Turno-act e)) #t #f))

(define (comparar l1 l2) (cond [(and (empty? l1) (empty? l2)) #t]
                               [(and (cons? l1) (empty? l2)) #f]
                               [(and (empty? l1) (cons? l2)) #f]
                               [#t (if (string=? (first l1) (first l2)) (and #t (comparar (rest l1) (rest l2))) #f)]))

(define (init l) (cond [(empty? l) l]
                       [(= 1 (length l)) empty]
                       [#t (cons (first l) (init (rest l)))]))

(define (final e) (cond [(even? (length (Turno-ant e)))
                         (place-image/align (text "Perdio Player 1" 20 "black") (/ ANCHO 2) (/ ALTO 2) "center" "center" ESCENA)]
                        [#t (place-image/align (text "Perdio Player 2" 20 "black") (/ ANCHO 2) (/ ALTO 2) "center" "center" ESCENA)]))

(define (keyboard e k) (cond [(and (= (length (Turno-ant e)) (length (Turno-act e))) (cons? (Turno-ant e)) (or (key=? k "up")
                                                                                                               (key=? k "down")
                                                                                                               (key=? k "left")
                                                                                                               (key=? k "right")))
                              (if (comparar (Turno-ant e) (Turno-act e))
                                  (make-Turno (agregarUltimo (Turno-act e) k) empty)
                                  (make-Turno (Turno-ant e) (cons "fin" empty)))]
                             [(and (key=? k "up") (empty? (Turno-ant e))) (make-Turno (cons "up" empty) empty)]
                             [(key=? k "up") (make-Turno (Turno-ant e) (agregarUltimo (Turno-act e) "up"))]
                             [(and (key=? k "down") (empty? (Turno-ant e))) (make-Turno (cons "down" empty) empty)]
                             [(key=? k "down") (make-Turno (Turno-ant e) (agregarUltimo (Turno-act e) "down"))]
                             [(and (key=? k "right") (empty? (Turno-ant e))) (make-Turno (cons "right" empty) empty)]
                             [(key=? k "right") (make-Turno (Turno-ant e) (agregarUltimo (Turno-act e) "right"))]
                             [(and (key=? k "left") (empty? (Turno-ant e))) (make-Turno (cons "left" empty) empty)]
                             [(key=? k "left") (make-Turno (Turno-ant e) (agregarUltimo (Turno-act e) "left"))]
                             [#t e]))

(big-bang ESTADO-INICIAL
  
  [to-draw draw]
  [on-key keyboard]
  [stop-when fin? final]
  
  )