#lang racket
(provide init-carta-en-juego)

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




