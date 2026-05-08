#lang racket
(provide init-carta-en-juego
         get-id
         get-carta-base
         get-cartas-asociadas
         get-energias
         get-dano
         get-estado)

; =============================
; ==== TDA CARTA-EN-JUEGO =====
; =============================

; ======= REPRESENTACIÓN ======
; Una lista de los siguientes 
; elementos:

; id -> int 
; carta-base -> Card 
; cartas-asociadas -> List
; energias -> List
; dano -> int
; estado -> String


; =============================
; ======== CONSTRUCTOR ========
; =============================
(define (init-carta-en-juego id carta)
        (list id carta '() '() 0 "normal"))

; =============================
; ======== SELECTORES =========
; =============================
(define (get-id cartaEnJuego)
        (list-ref cartaEnJuego 0))

(define (get-carta-base cartaEnJuego)
        (list-ref cartaEnJuego 1))

(define (get-cartas-asociadas cartaEnJuego)
        (list-ref cartaEnJuego 2))

(define (get-energias cartaEnJuego)
        (list-ref cartaEnJuego 3))

(define (get-dano cartaEnJuego)
        (list-ref cartaEnJuego 4))

(define (get-estado cartaEnJuego)
        (list-ref cartaEnJuego 5))


