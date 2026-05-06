;; Enumeración de tipos de carta
(define CARD-TYPE '(pokemon energy trainer))
(define (card-type? t)
(and (symbol? t)
(member t CARD-TYPE)))
;; Enumeración de elementos
(define ELEMENT-TYPE '(grass fire water lightning psychic fighting darkness metal colorless
fairy))
(define (element-type? e)
(and (symbol? e)
(member e ELEMENT-TYPE)))

;; Lista de nombres de energía con su tipo. Esta lista base se puede ajustar con otros
;; nombres de ataques y daños. Los tipos se limitan a los indicados por ELEMENT-TYPE
(define ENERGY '((fire-energy . fire) (water-energy . water) (grass-energy . grass)
(lightning-energy . lightning) (psychic-energy . psychic) (fighting-energy . fighting)
(darkness-energy . darkness) (metal-energy . metal) (fairy-energy . fairy) (colorless-energy .
colorless)))


; ==================================================================
; ================== FUNCIONES AUXILIARES ========================== 
; ==================================================================

; remover-por-indice: elimina el elemento de una lista basándose en su índice.
; Dom: List X int
; Rec: List
(define (remover-por-indice lista i)
        (cond 
              [(null? lista) '()]
              [(< i 0) lista]
              [(= i 0) (cdr lista)]
              [else 
                    (cons (car lista) (remover-por-indice (cdr lista) (- i 1)))]))

; randomPuro: genera números pseudoaleatorios .
; Dom: int
; Rec: int
(define (randomPuro Xn)
   (modulo (+ (* Xn 1103515245) 12345) 2147483648))





; ==================================================================
; ================== REQUERIMIENTOS FUNCIONALES ====================
; ==================================================================

; RF02
; Dom:
; Rec:
(define (attack cost nombre texto funcion-daño)
        (initAttack cost nombre texto funcion-daño))


; RF03
; Dom:
; Rec:
(define (card tipoCarta nombre . atributos-especificos)
        (initCard tipoCarta nombre atributos-especificos))


; RF04
; Dom:
; Rec:
(define (deck cartas)
        (initDeck cartas))


; RF05
; Dom:
; Rec:
(define (shuffleDeck mazo seed)
        (cond
             [(null? mazo) '()]
             [else 
                   (let* ((semilla (randomPuro seed))
                        (index (modulo semilla (length mazo))))
                        
                        (cons 
                              (list-ref mazo index) (shuffleDeck (remover-por-indice mazo index) semilla)))]))


; RF06
; Dom:
; Rec: 
(define (initGame mazo1 mazo2 seed)
        ())










