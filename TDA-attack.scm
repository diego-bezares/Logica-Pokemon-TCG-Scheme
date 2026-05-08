#lang racket
(provide initAttack
         attack?
         get-cost
         get-nombre
         get-descipcion
         get-fn
         set-cost
         set-nombre
         set-descripcion
         set-fn)

; =============================
; ========= TDA ATTACK ========
; =============================

; ======= REPRESENTACIÓN ======
; Una lista de los siguientes 
; elementos:

; cost -> List ELEMENT-TYPE
; nombre -> String
; descripción -> String
; función-daño -> F


; ========= CONSTRUCTOR =======
(define (initAttack cost nombre descripcion fn) 
        (list cost nombre descripcion fn))



; ========= SELECTORES ========
(define (get-cost attack)
        (list-ref attack 0))
(define (get-nombre attack)
        (list-ref attack 1))
(define (get-descripcion attack)
        (list-ref attack 2))
(define (get-fn attack)
        (list-ref attack 3))


; ======= MODIFICADORES =======
(define (set-cost attack nw-cost)
        (list nw-cost (get-nombre attack) (get-descipcion attack) (get-fn attack)))
(define (set-nombre attack nw-nombre)
        (list (get-cost attack) nw-nombre (get-descipcion attack) (get-fn attack)))
(define (set-descripcion attack nw-descripcion)
        (list (get-cost attack) (get-nombre attack) nw-descripcion (get-fn attack)))
(define (set-fn attack nw-fn)
        (list (get-cost attack) (get-nombre attack) (get-descipcion attack) nw-fn))










