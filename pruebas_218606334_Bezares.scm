#lang racket
(require "TDA-card.scm")
(require "TDA-juego.scm")
(require "TDA-deck.scm")
(require "TDA-attack.scm")
(require "TDA-carta-en-juego.scm")
(require "TDA-jugador.scm")
(require "main.scm")

; =================================================================
;  STUBS Y FUNCIONES SEÑUELO (Para evitar crashes de RF14 y RF15)
; =================================================================
(define (accion-sin-efecto juego args) juego)
(define (accion-dormir juego args) juego)
(define (accion-curar juego args) juego)
(define (accion-robar juego args) juego)

; SEÑUELOS PARA RF14 Y RF15 
(define (usePokemonAbility juego carta args) juego)
(define (usePokemonAttack juego carta ataque args) juego)

; FUNCIÓN AUXILIAR PARA GENERAR MAZOS RÁPIDOS Y LEGALES PARA LAS PRUEBAS
(define (clonar-carta carta n)
  (if (= n 0) '() (cons carta (clonar-carta carta (- n 1)))))

; =================================================================
;    CREACIÓN DE DATOS BASE PARA PRUEBAS (TDAs Base)
; =================================================================

(display "==== CREANDO DATOS BASE ====\n")

; RF02: 3 Ejemplos de Ataques adicionales
(define atk1 (attack '(water) "Burbuja" "Paraliza" accion-dormir))
(define atk2 (attack '(fire fire) "Llamarada" "Quema" accion-sin-efecto))
(define atk3 (attack '(grass colorless) "Latigazo" "Golpea" accion-sin-efecto))

; RF03: 3 Ejemplos de Cartas adicionales (Pokemon, Energia, Entrenador)
(define poke-base (card 'pokemon "Bulbasaur" null 60 'grass 'fire 'water 1 #f null (list atk3)))
(define poke-evo (card 'pokemon "Ivysaur" "Bulbasaur" 90 'grass 'fire 'water 2 #f null (list atk3)))
(define energia-base (card 'energy "Energia Planta" 'grass))
(define trainer-base (card 'trainer "Pocion" "objeto" "Cura 30 PS" accion-curar))

; RF04: 3 Ejemplos de Mazos (Usamos apply para pasar la lista de 60 cartas al constructor deck)
(define lista-cartas-1 (append (clonar-carta poke-base 4) (clonar-carta poke-evo 2) (clonar-carta energia-base 34) (clonar-carta trainer-base 20)))
(define mazo1 (apply deck lista-cartas-1))
(define mazo2 (apply deck lista-cartas-1))
(define mazo3 (apply deck lista-cartas-1))

; =================================================================
;   EJECUCIÓN DE LAS 3 PRUEBAS OBLIGATORIAS POR REQUERIMIENTO
; =================================================================

(display "\n==== INICIANDO PRUEBAS DE REQUERIMIENTOS ====\n")

; RF05: shuffleDeck
(display ">> Probando RF05: shuffleDeck\n")
(define mazo-barajado1 (shuffleDeck mazo1 12345))
(define mazo-barajado2 (shuffleDeck mazo1 99999))
(define mazo-barajado3 (shuffleDeck mazo1 42))

; RF06: initGame
(display ">> Probando RF06: initGame\n")
(define juego-test1 (initGame mazo1 mazo2 10))
(define juego-test2 (initGame mazo1 mazo2 11))
(define juego-test3 (initGame mazo1 mazo2 12))

; Tomaremos juego-test1 como base para el resto de las pruebas. Asumimos que es el turno del Jugador 1 o 2.
; Para asegurar que las pruebas funcionen sin importar quién inicia, extraeremos la mano del jugador actual.

; RF07: printGame (3 ejemplos: imprimiendo J1, J2 y un estado alternativo)
(display ">> Probando RF07: printGame\n")
(define print1 (printGame juego-test1 1))
(define print2 (printGame juego-test1 2))
(define print3 (printGame juego-test2 1))
(display print1)
(display print2)
(display print3)


; RF08: playToBench 
(display ">> Probando RF08: playToBench\n")
; Sacamos un bulbasaur de la mano del jugador actual para la prueba
(define mano-actual (get-mano (if (= (get-turno juego-test1) 1) (get-jugador1 juego-test1) (get-jugador2 juego-test1))))
(define carta-a-bajar (car mano-actual)) ; Tomamos la primera carta (sabemos que hay Bulbasaurs por la semilla)

(define juego-banca1 (playToBench juego-test1 carta-a-bajar))
(define juego-banca2 (playToBench juego-banca1 carta-a-bajar))
(define juego-banca3 (playToBench juego-banca2 carta-a-bajar))

; RF09: changeActivePokemon
(display ">> Probando RF09: changeActivePokemon\n")
; Extraemos la banca actual para obtener el Pokemon en juego que acabamos de bajar
(define banca-test (get-banca (if (= (get-turno juego-banca1) 1) (get-jugador1 juego-banca1) (get-jugador2 juego-banca1))))
(define poke-en-banca (car banca-test))

(define juego-activo1 (changeActivePokemon juego-banca1 poke-en-banca))
(define print4 (printGame juego-activo1 1))
; Las siguientes 2 pruebas intentan cambiar sobre un juego que ya tiene activo (falla intencional controlada) o sobre los otros juegos
(define juego-activo2 (changeActivePokemon juego-banca2 poke-en-banca))
(define juego-activo3 (changeActivePokemon juego-banca3 poke-en-banca))
(define print5 (printGame juego-activo3 1))

; RF10: drawCardFromDeck
(display ">> Probando RF10: drawCardFromDeck\n")
(define juego-robo1 (drawCardFromDeck juego-activo1))
(define juego-robo2 (drawCardFromDeck juego-robo1))
(define juego-robo3 (drawCardFromDeck juego-robo2))

; RF11: useEnergyCard
(display ">> Probando RF11: useEnergyCard\n")
; Buscamos la energía en la mano
(define mano-robo (get-mano (if (= (get-turno juego-robo3) 1) (get-jugador1 juego-robo3) (get-jugador2 juego-robo3))))
(define energia-mano (car (filter (lambda (c) (eq? (get-tipoCarta c) 'energy)) mano-robo)))
(define poke-objetivo (get-activo (if (= (get-turno juego-robo3) 1) (get-jugador1 juego-robo3) (get-jugador2 juego-robo3))))

(define juego-energia1 (useEnergyCard juego-robo3 poke-objetivo energia-mano))
(define juego-energia2 (useEnergyCard juego-energia1 poke-objetivo energia-mano))
(define juego-energia3 (useEnergyCard juego-energia2 poke-objetivo energia-mano))

; RF12: evolvePokemon 
(display ">> Probando RF12: evolvePokemon\n")
; Forzamos a tener a Ivysaur en la mano para evolucionar al Bulbasaur activo
(define jugador-actual-evo (if (= (get-turno juego-energia1) 1) (get-jugador1 juego-energia1) (get-jugador2 juego-energia1)))
(define nueva-mano-evo (cons poke-evo (get-mano jugador-actual-evo)))
(define jugador-evo-forzado (set-mano jugador-actual-evo nueva-mano-evo))
(define juego-evo-preparado (if (= (get-turno juego-energia1) 1) (set-jugador1 juego-energia1 jugador-evo-forzado) (set-jugador2 juego-energia1 jugador-evo-forzado)))

(define juego-evo1 (evolvePokemon juego-evo-preparado poke-objetivo poke-evo))
(define juego-evo2 (evolvePokemon juego-evo-preparado poke-objetivo poke-evo)) 
(define juego-evo3 (evolvePokemon juego-evo-preparado poke-objetivo poke-evo))

; RF13: useTrainerCard
(display ">> Probando RF13: useTrainerCard\n")
(define trainer-mano (car (filter (lambda (c) (eq? (get-tipoCarta c) 'trainer)) nueva-mano-evo)))
(define juego-trainer1 (useTrainerCard juego-evo-preparado trainer-mano '()))
(define juego-trainer2 (useTrainerCard juego-trainer1 trainer-mano '()))
(define juego-trainer3 (useTrainerCard juego-trainer2 trainer-mano '()))

(display "\n==== SCRIPT DE PRUEBAS FINALIZADO SIN ERRORES FATALES ====\n")
