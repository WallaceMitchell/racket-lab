;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |practica 6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;ejercicio 1

(define (sumanat x y) (cond [(zero? x) y]
                            [(positive? x) (sumanat (sub1 x) (add1 y))]
                            [#t "Tipos de datos incorrectos"]))

;ejercicio 2

(define (multnat x y) (cond [(zero? x) 0]
                            [(zero? (sub1 x)) y]
                            [(positive? x) (sumanat y (multnat (sub1 x) y))]
                            [#t "Tipos de datos incorrectos"]))

;ejercicio 3

(define (powernat x y) (cond [(zero? y) 1]
                             [(zero? (sub1 y)) x]
                             [(positive? y) (multnat x (powernat x (sub1 y)))]
                             [#t "Tipos de datos incorrectos"]))

;ejercicio 4

(define (factnat x) (cond [(zero? x) 1]
                          [(zero? (sub1 x)) 1]
                          [(positive? x) (multnat x (factnat (sub1 x)))]))

;ejercicio 5

(define (fibnat x) (cond [(zero? x) 1]
                         [(zero? (sub1 x)) 1]
                         [(positive? x) (sumanat (fibnat (sub1 x)) (fibnat (sub1 (sub1 x))))]))

;ejercicio 6

(define (sigma n f) (cond [(zero? n) (f 0)]
                          [(positive? n) (+ (f n) (sigma (sub1 n) f))]))

;ejercicio 7

(define (R n) (cond [(zero? n) 1]
                    [(positive? n) (+ (/ 1 (expt 2 n)) (R (sub1 n)))]))

(define (S n) (cond [(zero? n) 0]
                    [(positive? n) (+ (/ n (sumanat (sub1 n) n) (S (sub1 n))))]))

(define (T n) (cond [(zero? n) 1]
                    [(positive? n) (+ (/ 1 (sumanat n 1)) (T (sub1 n)))])) ;casos de prueba los debo, pedirselos a chatgpt

;ejercicio 8

(define (componer f n x) (cond [(zero? n) x]
                               [(positive? n) (componer f (sub1 n) (f x))]))

;ejercicio 9

(define (intervalo n) (cond [(zero? n) (cons 0 '())]
                            [(positive? n) (cons n (intervalo (sub1 n)))]))

;ejercicio 10

(define (multiplos n m) (cond [(zero? n) '()]
                              [(positive? n) (cons (* m n) (multiplos (sub1 n) m))]))

;ejercicio 11

(define (list-fib n) (cond [(zero? n) (cons 1 '())]
                           [(positive? n) (cons (fibnat n) (list-fib (sub1 n)))]))

(define (fibo-list n) (map fibnat (intervalo n)))

;ejercicio 12
(define n 3)
(define (cuotas t j i) (cond [(zero? j) empty]
                             [(positive? j) (cons (+ (/ t n) (* (/ t n) (/ i (* 100 12)) j)) (cuotas t (sub1 j) i))]))

;ejercicio 13

(define (circulos m) (local
                       (
                        (define (circ x) (cond [(zero? x) (square (* 2 (sqr m)) "outline" "black")]
                                               [(positive? x) (place-image (circle (sqr x) "outline" "blue") (sqr m) (sqr m) (circ (sub1 x)))])))
                       (circ m)))

;ejercicio 14

(define LADO 200)
(define (cuadrados m ang) (cond [(zero? m) (square LADO "outline" "black")]
                                [(positive? m) (place-image (rotate ang (square (sqr m) "outline" "blue")) (/ LADO 2) (/ LADO 2) (cuadrados (sub1 m) (+ ang 20)))]))

;ejercicio 15

(define (list-insert l x i) (cond [(zero? i) (cons x l)]
                                  [(positive? x) (cond [(>= i (length l)) (append l (cons x '()))]
                                                       [#t (cons (first l) (list-insert (rest l) x (sub1 i)))])]))

;ejercicio 16

(define (tomar l n) (cond [(zero? n) empty]
                          [(positive? n) (cond [(>= n (length l)) l]
                                               [#t (cons (first l) (tomar (rest l) (sub1 n)))])]))

;ejercicio 17

(define (distintos x l) (cond [(empty? l) empty]
                              [(cons? l) (if (= (first l) x) (distintos x (rest l)) (cons (first l) (distintos x (rest l))))]))

(define (eliminar l e n) (cond [(zero? n) (if (= (first l) e) (rest l) l)]
                               [(>= n (length l)) (distintos e l)]
                               [(positive? n) (if (= (first l) e) (eliminar (rest l) e (sub1 n)) (cons (first l) (eliminar (rest l) e (sub1 n))))]))

;ejercicio 18

(define (iguales x l) (cond [(empty? l) l]
                            [(cons? l) (if (= (first l) x) (cons (first l) (iguales x (rest l))) (iguales x (rest l)))]))

(define (list=? l1 l2) (cond [(and (empty? l1) (empty? l2)) #t]
                             [(empty? l1) #f]
                             [(empty? l2) #f]
                             [#t (if (= (first l1) (first l2)) (and #t (list=? (rest l1) (rest l2))) #f)]))

(define (member-n l x n) (cond [(> n (length l)) #f]
                               [(zero? n) (if (list=? empty (iguales x l)) #t #f)]
                               [(positive? n) (= n (length (iguales x l)))]))




