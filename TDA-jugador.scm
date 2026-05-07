#lang racket
(provide init-jugador
         get-mazo
         get-mano
         get-activo
         get-banca
         get-descarte
         get-premios)

; =============================
; ======== TDA JUGADOR ========
; =============================

; ======= REPRESENTACIÓN ======
; Una lista de los siguientes 
; elementos:

; mazo -> List (deck)
; mano -> List (card)
; poke activo -> Card o null
; banca -> List (card)
; descarte -> List (card)
; premios -> List (card)



; ========= CONSTRUCTOR =======
(define (init-jugador mazo mano activo banca descarte premios)
        (list mazo mano activo banca descarte premios))

; ========= SELECTORES ========
(define (get-mazo jugador)
        (list-ref jugador 0))

(define (get-mano jugador)
        (list-ref jugador 1))

(define (get-activo jugador)
        (list-ref jugador 2))

(define (get-banca jugador)
        (list-ref jugador 3))

(define (get-descarte jugador)
        (list-ref jugador 4))

(define (get-premios jugador)
        (list-ref jugador 5))
