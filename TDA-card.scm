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

; == FUNCIÓN DE PERTENENCIA ==
(define (card? card)
        (and (list? card)
             (string? (get-nombre-card card))
             (card-type? (get-tipoCarta card))
             (cond 
                   [(eq? (get-tipoCarta card) 'pokemon)
                         (and (string? (get-evo card))
                              (and (integer? (get-ps card)) (> (get-ps card) 0))
                              (element-type? (get-tipo-poke card))
                              (element-type? (get-debilidad card))
                              (element-type? (get-resistencia card))
                              (integer? (get-coste-ret card))
                              (boolean? (get-ex card))
                              (attack? (get-habilidad card))
                              (map (lambda (x) (attack? x)) (get-ataques card)))]
                   [(eq? (get-tipoCarta card) 'trainer)
                         (and (string? (get-trainerType card))
                              (string? (get-description card))
                              (procedure? (get-accion card)))])))

; ========= SELECTORES ========
(define (get-tipoCarta card)
        (car card))
(define (get-nombre-card card)
        (cadr card))
(define (get-evo card)
        (if (eq? (get-tipoCarta card) 'pokemon)
            (caddr card)
            (write "El tipo de carta no contiene este elemento.")))
(define (get-trainerType card)
        (if (eq? (get-tipoCarta card) 'trainer)
            (caddr card)
            (write "El tipo de carta no contiene este elemento.")))
(define (get-ps card)
        (if (eq? (get-tipoCarta card) 'pokemon)
            (cadddr card)
            (write "El tipo de carta no contiene este elemento.")))
(define (get-description card)
        (if (eq? (get-tipoCarta card) 'trainer)
            (cadddr card)
            (write "El tipo de carta no contiene este elemento.")))
(define (get-tipo-poke card)
        (if (eq? (get-tipoCarta card) 'pokemon)
            (car (cddddr card))
            (write "El tipo de carta no contiene este elemento.")))
(define (get-accion card)
        (if (eq? (get-tipoCarta card) 'trainer)
            (car (cddddr card))
            (write "El tipo de carta no contiene este elemento.")))
(define (get-debilidad card)
        (if (eq? (get-tipoCarta card) 'pokemon)
            (cadr (cddddr card))
            (write "El tipo de carta no contiene este elemento.")))
(define (get-resistencia card)
        (if (eq? (get-tipoCarta card) 'pokemon)
            (caddr (cddddr card))
            (write "El tipo de carta no contiene este elemento.")))
(define (get-coste-ret card)
        (if (eq? (get-tipoCarta card) 'pokemon)
            (cadddr (cddddr card))
            (write "El tipo de carta no contiene este elemento.")))
(define (get-ex card)
        (if (eq? (get-tipoCarta card) 'pokemon)
            (car (cddddr (cddddr card)))
            (write "El tipo de carta no contiene este elemento.")))
(define (get-habilidad card)
        (if (eq? (get-tipoCarta card) 'pokemon)
            (cadr (cddddr (cddddr card)))
            (write "El tipo de carta no contiene este elemento.")))
(define (get-ataques card)
        (if (eq? (get-tipoCarta card) 'pokemon)
            (caddr (cddddr (cddddr card)))
            (write "El tipo de carta no contiene este elemento.")))

; ======== MODIFICADORES =======
























