#lang racket
(provide init-juego
         get-jugador1
         get-jugador2
         get-turno-actual)

; =============================
; ========== TDA JUEGO ========
; =============================

; ======= REPRESENTACIÓN ======
; Una lista de los siguientes 
; elementos:

; jugador1 -> Jugador
; jugador2 -> Jugador
; turno-actual -> int



; ========= CONSTRUCTOR =======
(define (init-juego jugador1 jugador2 turno-actual)
        (list jugador1 jugador2 turno-actual))

; ========= SELECTORES ========
(define (get-jugador1 juego)
        (list-ref juego 0))
(define (get-jugador2 juego)
        (list-ref juego 1))
(define (get-turno-actual juego)
        (list-ref juego 2))
























