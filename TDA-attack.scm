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


; === FUNCIÓN DE PERTENENCIA ==
(define (attack? attack)
        (and 
             (list? attack)
             (= 4 (length attack))
             (list? (car attack))
             (null? (member #f (map (lambda (e) (element-type? e)) (car attack))))
             (string? nombre)
             (string? descripcion)
             (procedure? fn)))


; ========= SELECTORES ========
(define (get-cost attack)
        (car attack))
(define (get-nombre attack)
        (cadr attack))
(define (get-descipcion attack)
        (caddr attack))
(define (get-fn attack)
        (cadddr attack))


; ======= MODIFICADORES =======
(define (set-cost attack nw-cost)
        (list nw-cost (get-nombre attack) (get-descipcion attack) (get-fn attack)))
(define (set-nombre nw-nombre)
        (list (get-cost attack) nw-nombre (get-descipcion attack) (get-fn attack)))
(define (set-descripcion nw-descripcion)
        (list (get-cost attack) (get-nombre attack) nw-descripcion (get-fn attack)))
(define (set-fn attack nw-fn)
        (list (get-cost attack) (get-nombre attack) (get-descipcion attack) nw-fn))










