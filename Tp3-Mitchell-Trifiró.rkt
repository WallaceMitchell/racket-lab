;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Tp3-Mitchell-Trifiró) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; representaremos alfabetos como Strings.
; Por ejemplo, si nuestros símbolos son las cinco primeras letras, los dígitos y el espacio,
; lo representaremos como "ABCDE0123456789 "

; representaremos símbolos como strings de longitud 1. En el alfabeto anterior,
; el símbolo E lo representamos con el string "E"

; el código del césar lo representaremos mediante parejas de símbolos.
; Por ejemplo, si queremos decir que el símbolo "A" se codifica con el
; símbolo "C", tendremos (make-Tupla "A" "C") para representar esta situación.

;;;;;;;; Primero comenzamos definiendo algunas funciones
; sobre strings y listas que nos son de utilidad.

; partir : String -> Listof String
; dado un string, devuele una lista de strings con cada símbolo separado

(check-expect (partir "ABC") (list "A" "B" "C"))
(check-expect (partir "12345") (list "1" "2" "3" "4" "5"))
(check-expect (partir "") empty)

(define (partir s) (cond [(string=? "" s) '()]
                         [#t (cons (substring s 0 1) (partir (substring s 1 (string-length s))))]))

; tomar : (Listof Natural) Natural -> (Listof Natural)
; dada una lista y un número natural n, devuelve una lista
; con los primeros n elementos de l. Si l no tiene tantos elementos,
; devuelve l.

(check-expect (tomar (list 1 2 3 4 5) 4) (list 1 2 3 4))
(check-expect (tomar (list 1 2 3 4 5) 10) (list 1 2 3 4 5))
(check-expect (tomar (list 1 2 3 4 5) 0) empty)
(check-expect (tomar empty 5) empty)


(define (tomar l n) (cond [(or (zero? n) (empty? l)) empty]
                          [else (cons (first l) (tomar (rest l) (sub1 n)))]))

; tirar : (Listof Natural) Natural -> Listof Natural
; dada una lista y un número natural n, devuelve una lista
;  sin los primeros n elementos de l. Si l no tiene tantos elementos,
; devuelve empty.

(check-expect (tirar (list 1 2 3 4 5) 2) (list 3 4 5))
(check-expect (tirar (list 1 2 3 4 5) 10) empty)
(check-expect (tirar (list 1 2 3 4 5) 0) (list 1 2 3 4 5))
(check-expect (tirar empty 3) empty)

(define (tirar l n) (cond [(empty? l) '()]
                          [(cons? l) (cond [(>= n (length l)) '()]
                                           [#t (if (= n 0) l (tirar (rest l) (- n 1)))])]))

; OBSERVACION: para cualquier n <= length l, (append (tomar n l) (tirar n l)) = l

(define-struct Tupla [f s])
; Tupla es [Any Any]
; que representa un par de elementos de cualquier tipo.  

; emparejar : (Listof X) (Listof Y) -> Listof Tuplas
; dadas dos listas [a0,..., an] y [b0, ...., bn] de la misma longitud, devuelve una lista
; de tuplas con parejas tomadas de ambas listas: [(make-Tupla a0 b0), ...., (make-Tupla an bn)]
(check-expect (emparejar (list "a" "b") (list 3 4)) (list (make-Tupla "a" 3) (make-Tupla "b" 4)))
(check-expect (emparejar (list "h" "l") (list "o" "a")) (list (make-Tupla "h" "o") (make-Tupla "l" "a")))

(define (emparejar l1 l2) (cond [(empty? l1) empty]
                                [else (cons
                                       (make-Tupla (first l1) (first l2))
                                       (emparejar (rest l1) (rest l2)))]))


;;;;;;;;;;;;; Ahora comienzan las funciones específicas para el método del César
; cifrado : N String -> Listof Tupla
; dada una clave de desplazamiento y un alfabeto s, devuelve una lista
; con parejas de strings, donde el primer elemento es el caracter a cifrar, y el segundo
; su código del César de acuerdo a la clave. Se asume que 0 < n < (string-length s)

(check-expect (cifrado 2 "ABC") (list (make-Tupla "A" "C") (make-Tupla "B" "A") (make-Tupla "C" "B"))) 
(check-expect (cifrado 1 "ABC") (list (make-Tupla "A" "B") (make-Tupla "B" "C") (make-Tupla "C" "A")))
; COMPLETAR (las funciones tirar, tomar, partir y emparejar le serán de utilidad.

(define (cifrado n s) (cond [(string=? s "") '()]
                            [#t (emparejar (partir s) (append (tirar (partir s) n) (tomar (partir s) n)))]))



; encriptar-simbolo : String (Listof Tupla) -> String
; dado un string s de longitud 1 que es un símbolo del
; alfabeto y una lista de parejas que representa un código del césar,
; devuelve el código que le corresponde a s

(check-expect (encriptar-simbolo "A" (cifrado 2 "ABC")) "C")
(check-expect (encriptar-simbolo "A" (cifrado 1 "ABC")) "B")

(define (encriptar-simbolo s l) (cond [(string=? s (Tupla-f (first l))) (Tupla-s (first l))]
                                      [else (encriptar-simbolo s (rest l))]))


; encriptar-mensaje : String String Natural -> String
; dado un string, un alfabeto y una clave, devuelve el string encriptado
(check-expect (encriptar-mensaje "ABC" "ABCDEF" 3) "DEF")
(check-expect (encriptar-mensaje "ABC" "ABCDEF" 4) "EFA")

(define (encriptar-mensaje s a n) (cond [(string=? s "") ""]
                                        [else (string-append
                                               (encriptar-simbolo (substring s 0 1) (cifrado n a))
                                               (encriptar-mensaje (substring s 1) a n))]))


; desencriptar-simbolo : String (Listof Tupla) -> String
; dado un string s de longitud 1 que es un símbolo del
; alfabeto y una lista de parejas que representa un código del césar,
; devuelve el caracter desencriptado que le corresponde a s

(check-expect (desencriptar-simbolo "A" (cifrado 2 "ABC")) "B")
(check-expect (desencriptar-simbolo "A" (cifrado 1 "ABC")) "C")

(define (desencriptar-simbolo s c) (cond [(= (string-length s) 1) (cond [(empty? c) ""]
                                                                        [(cons? c) (if (string=? (Tupla-s (first c)) s) (Tupla-f (first c)) (desencriptar-simbolo s (rest c)))])]
                                         [#t "Largo de la cadena o tipos de datos incorrectos."]))

; desencriptar-mensaje : String String Natural -> String
; dado un string, un alfabeto y una clave, devuelve el string encriptado

(check-expect (desencriptar-mensaje "DEF" "ABCDEF" 3) "ABC")
(check-expect (desencriptar-mensaje "EFA" "ABCDEF" 4) "ABC")

(define (desencriptar-mensaje s a n) (cond [(string=? s "") ""]
                                        [else (string-append
                                               (desencriptar-simbolo (substring s 0 1) (cifrado n a))
                                               (desencriptar-mensaje (substring s 1) a n))]))

