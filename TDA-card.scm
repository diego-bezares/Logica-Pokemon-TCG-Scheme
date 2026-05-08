#lang racket
(provide initCard
         get-tipoCarta
         get-nombre-card
         get-evo
         get-trainerType
         get-ps
         get-description
         get-tipo-poke
         get-accion
         get-debilidad
         get-resistencia
         get-coste-ret
         get-ex
         get-habilidad
         get-ataques)

; =============================
; ========== TDA CARD =========
; =============================

; ======= REPRESENTACIÓN ======
; Una lista de n elementos 
; dependiendo del tipo de carta:

; POKEMON TYPE:
; tipoCarta -> CARD-TYPE
; nombre -> String
; EvolucionDe -> String
; PS -> int+
; tipoPokemon -> ELEMENT-TYPE
; Debilidad -> ELEMENT-TYPE
; Resistencia -> ELEMENT-TYPE
; CosteRetirada -> int
; EsEX -> Boolean
; habilidad -> Attack
; ataques -> List Attack

; ENERGY TYPE:
; tipoCarta -> CARD-TYPE
; nombre -> String

; TRAINER TYPE:
; tipoCarta -> CARD-TYPE
; nombre -> String
; trainerType -> String
; descripcion -> String
; fnAccion -> F

; ======= CONSTRUCTOR ========
(define (initCard tipoCarta nombre . atributos-especificos)
        (cons tipoCarta (cons nombre atributos-especificos)))

; ========= SELECTORES ========
(define (get-tipoCarta card)
        (list-ref card 0))
(define (get-nombre-card card)
        (list-ref card 1))
(define (get-evo card)
        (list-ref card 2))
(define (get-trainerType card)
        (list-ref card 2))
(define (get-ps card)
        (list-ref card 3))
(define (get-description card)
        (list-ref card 3))
(define (get-tipo-poke card)
        (list-ref card 4))
(define (get-accion card)
        (list-ref card 4))
(define (get-debilidad card)
        (list-ref card 5))
(define (get-resistencia card)
        (list-ref card 6))
(define (get-coste-ret card)
        (list-ref card 7))
(define (get-ex card)
        (list-ref card 8))
(define (get-habilidad card)
        (list-ref card 9))
(define (get-ataques card)
        (list-ref card 10))
























