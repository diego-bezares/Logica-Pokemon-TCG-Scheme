#lang racket
(provide init-juego
         get-jugador1
         get-jugador2
         get-turno
         set-jugador1
         set-jugador2
         set-turno)

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

(define (get-turno juego)
        (list-ref juego 2))

; ======== MODIFICADORES =======
(define (set-jugador1 juego nuevoJugador1)
        (init-juego nuevoJugador1 (get-jugador2 juego) (get-turno juego)))

(define (set-jugador2 juego nuevoJugador2)
        (init-juego (get-jugador1 juego) nuevoJugador2 (get-turno juego)))

(define (set-turno juego nuevoTurno)
        (init-juego (get-jugador1 juego) (get-jugador2 juego) nuevoTurno))
























