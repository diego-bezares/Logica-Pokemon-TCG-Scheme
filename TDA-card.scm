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




; ======= CONSTRUCTOR =======
(define (initCard tipoCarta nombre . atributos-especificos)
        (cons tipoCarta (cons nombre atributos-especificos)))

; = FUNCIÓN DE PERTENENCIA ==













