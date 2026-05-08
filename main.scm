#lang racket

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

; randomPuro: genera números pseudoaleatorios.
; Dom: int
; Rec: int
(define (randomPuro Xn)
   (modulo (+ (* Xn 1103515245) 12345) 2147483648))

; tomar-n: devuelve los n primeros elementos de una lista.
; Dom: List X int
; Rec: List
(define (tomar-n lista n)
        (cond 
              [(= n 0) '()]
              [(> n 0) 
                       (cons (car lista) (tomar-n (cdr lista) (- n 1)))]))

; botar-n: devuelve la lista sin los primeros n elementos.
; Dom: List X int
; Rec: List
(define (botar-n lista n)
        (cond
             [(> n 0) (botar-n (cdr lista) (- n 1))]
             [(and (not (null? lista)) (= n 0)) (cons (car lista) (botar-n (cdr lista) n))]
             [(null? lista) '()]))

; es-pokemon-basico?: devuelve #t si la carta es de tipo pokemon y el campo "EvolucionDe" es null.
; Dom: Card
; Rec: boolean
(define (es-pokemon-basico? carta)
        (and (eq? (get-tipoCarta carta) 'pokemon) (null? (get-evo carta))))

; preparar-jugador: toma un mazo (60 cartas) y prepara al jugador, es decir, hace la mano, verificando que haya al menos un pokemon basico, 
; deja las cartas restantes, saca premios y deja la banca vacía al igual que los descartes para devolver un Jugador.
; Dom: Deck X Semilla (int)
; Rec: Jugador
(define (preparar-jugador mazo seed)
        (let* ((mazo-barajado (shuffleDeck mazo seed))
               (mano-tentativa (tomar-n mazo-barajado 7)))
              
              (if 
                  (ormap (lambda (x) (es-pokemon-basico? x)) mano-tentativa)
                  ; CASO VERDADERO: Hay pokemon básico, se inicia al jugador.
                  (let* ((mazo-sin-mano (botar-n mazo-barajado 7))
                         (premios (tomar-n mazo-sin-mano 6))
                         (mazo-final (botar-n mazo-sin-mano 6)))
                    
                         (init-jugador mazo-final mano-tentativa null '() '() premios))
                  ; CASO FALSO: No hay pokemon básico, se vuelve a barajar.
                  (preparar-jugador mazo (randomPuro seed)))))

; formatear-string-opcional: formatea el campo de EvolucionDe que tenga un null para que no fallen los string-append
; Dom: Symbol o null
; Rec: String
(define (formatear-string-opcional valor)
        (if (null? valor)
            "Básico"
            valor))

; formatear-simbolo-opcional: formatea el campo de Resistencia o Debilidad que tenga un null para que no fallen los string-append
; Dom: Symbol o null
; Rec: String
(define (formatear-string-opcional valor)
        (if (null? valor)
            "Ninguna"
            valor))


; formatear-una-carta: transforma los atributos de la carta en un string.
; Dom: Card
; Rec: String
(define (formatear-una-carta carta)
        (cond
        ; CASO 1: La carta es un pokemon.
             [(eq? (get-tipoCarta carta) 'pokemon)
              (let* (; Se extraen los atributos.
                     (nombre (get-nombre-card carta))
                     (evo-texto (formatear-string-opcional (get-evo carta)))
                     (ps-texto (number->string (get-ps carta)))
                     (tipo-texto (symbol->string (get-tipo-poke carta)))
                     (debilidad-texto (formatear-simbolo-opcional (get-debilidad carta)))
                     (resistencia-texto (formatear-simbolo-opcional (get-resistencia carta)))
                     ; Datos dinámicos
                     (energias-texto (string-append (number->string (length (get-energias carta))) " energías"))
                     (dano-texto (string-append (number->string (get-dano carta)) " daño"))
                     (estado-texto "normal") ;;***********
                    )
                    ; Construcción del string
                    (string-append
                                  "Nombre: " nombre " | " ps-texto " PS | Tipo: " tipo-texto "\n"
                                  "Etapa: " evo-texto "\n"
                                  "Debilidad: " debilidad-texto " | Resistencia: " resistencia-texto "\n"
                                  "Estado actual: " estado-texto " | " energias-texto " | " dano-texto "\n"
                                  "---------------------------------------------------------------------\n"))]
         
             ; CASO 2: La carta es energía.
             [(eq? (get-tipoCarta carta) 'energy)
              (string-append "Energía: " (symbol->string (list-ref carta 2)) "\n")]

             ; CASO 3: La carta es entrenador.
             [(eq? (get-tipoCarta carta) 'trainer)
              (string-append "Entrenador: " (get-nombre-card carta) "\n")]
         
             [else "Carta desconocida\n"]))

; iterar-cartas-cola: toma una lista de cartas y las transforma en un string una por una con recursión de cola
; para generar un string completo con todas las cartas.
; Dom: List X String X Function
; Rec: String
(define (iterar-cartas-cola lista acumulador func-formateadora)
  (cond
    ; Caso base: si la lista está vacía, entregas el string acumulado.
    [(null? lista) acumulador]
    
    ; Caso recursivo de cola: se llamas a la misma función, pero le pasa el resto de la lista 
    ; y al acumulador se le pegas el resultado de formatear la carta actual.
    [else 
     (iterar-cartas-cola (cdr lista)
                         (string-append acumulador (func-formateadora (car lista)))
                         func-formateadora)]))

; formatear-solo-nombre: toma una carta y solo saca su nombre para transformarla en string (para el descarte y la mano rival).
; Dom: Card
; Rec: String
(define (formatear-solo-nombre carta)
  (string-append "- " (get-nombre-card carta) "\n"))

; remover-primera-ocurrencia: recibe una carta y una lista de cartas (mano) y la primera carta que encuentre con el mismo nombre
; la elimina y devuelve el resto de la lista.
; Dom: Card X mano (List card)
; Rec: List card
(define (remover-primera-ocurrencia carta lista)
        (cond
             [(null? lista) '()]
             [(equal? (get-nombre-card carta) (get-nombre-card (car lista))) (cdr lista)]
             [else 
                   (cons (car lista) (remover-primera-ocurrencia carta (cdr lista)))]))

; contar-copias: toma un mazo y cuenta cuantas cartas hay con el mismo nombre que recibe.
; Dom: String
; Rec: int
(define (contar-copias nombre-buscado mazo)
  (length 
   (filter (lambda (c) (equal? (get-nombre-card c) nombre-buscado)) 
           mazo)))

; carta-es-legal?: reciba una carta y un mazo, si la carta es energía, retorna #t (no tienen límite), si no
; se verifica que la carta no pase el límite de 4 copias.
(define (carta-es-legal? carta mazo)
        (cond
             [(eq? (get-tipoCarta carta) 'energy) #t]
             [(> 4 (contar-copias (get-nombre-card carta) mazo)) #t]
             [else #f]))

; ==================================================================
; ==================================================================
; ==================================================================






; ==================================================================
; ================== REQUERIMIENTOS FUNCIONALES ====================
; ==================================================================

; RF02
; Dom: cost (List ELEMENT-TYPE) X nombre(string) X texto (string) X función de daño
; Rec: attack
(define (attack cost nombre texto funcion-daño)
        (initAttack cost nombre texto funcion-daño))


; RF03
; Dom: 
; Si es de tipo Pokémon:
; tipoCarta (CARD-TYPE) X nombre (string) X EvolucionaDe (string) X PS (int+) X tipoPokemon (ELEMENT-TYPE) X Debilidad (ELEMENT-TYPE) 
; X Resistencia (ELEMENT-TYPE) X CosteRetirada (int) X EsEX (boolean) X habilidad (Attack) X ataques (List attack)
; Si es de tipo energía:
; tipoCarta (CARD-TYPE) X nombre (string) X tipoEnergia (ELEMENT-TYPE)
; Si es de tipo entrenador:
; tipoCarta (CARD-TYPE) X nombre (string) X trainerType (string) X texto (string) X función de acción
; Rec: card
(define (card tipoCarta nombre . atributos-especificos)
        (initCard tipoCarta nombre atributos-especificos))


; RF04
; Dom: cartas (List card)
; Rec: deck
(define (deck . cartas)
  (cond
    ; Validamos tamaño
    [(not (= (length cartas) 60))
     (error "Mazo invalido: Debe tener exactamente 60 cartas.")]
     
    ; Validamos que exista al menos un básico
    [(not (ormap es-pokemon-basico? cartas))
     (error "Mazo invalido: Debe contener al menos un Pokémon básico.")]
     
    ; Validamos límite de 4 copias
    [(not (andmap (lambda (c) (carta-es-legal? c cartas)) cartas))
     (error "Mazo invalido: Supera el límite de 4 copias permitidas para una carta.")]
     
    ; ÉXITO: retornamos el mazo
    [else (initDeck cartas)]))


; RF05
; Dom: mazo (deck) X seed (int+)
; Rec: deck
(define (shuffleDeck mazo seed)
        (cond
             [(null? mazo) '()]
             [else 
                   (let* ((semilla (randomPuro seed))
                        (index (modulo semilla (length mazo))))
                        
                        (cons 
                              (list-ref mazo index) (shuffleDeck (remover-por-indice mazo index) semilla)))]))


; RF06
; Dom: mazo (deck) X mazo (deck) X seed (int+)
; Rec: Juego
(define (initGame mazo1 mazo2 seed)
        (let* ((seed-j1 seed)
               (seed-j2 (randomPuro (randomPuro (randomPuro seed))))
               (seed-moneda (randomPuro (randomPuro seed-j2)))
               
               (jugador1 (preparar-jugador mazo1 seed-j1))
               (jugador2 (preparar-jugador mazo2 seed-j2))
               (turno (modulo seed-moneda 2)))
               
               (init-juego jugador1 jugador2 turno)))


; RF07
; Dom: juego (Juego) X numero_jugador (int)
; Rec: string
(define (printGame juego numero_jugador)
        (let* (; Extraemos los jugadores y el turno del TDA Juego
              (jugador1 (get-jugador1 juego))
              (jugador2 (get-jugador2 juego))
              (turno-actual (get-turno juego))
         
              ; Extraemos datos del Jugador 1 y Jugador 2
              (mano-j1 (get-mano jugador1))
              (banca-j1 (get-banca jugador1))
              (descarte-j1 (get-descarte jugador1))
              (activo-j1 (get-activo jugador1))
              ; =========================================
              (mano-j2 (get-mano jugador2))
              (banca-j2 (get-banca jugador2))
              (descarte-j2 (get-descarte jugador2))
              (activo-j2 (get-activo jugador2))
         
              ; Procesamos la mano según "numero_jugador"
              (string-mano-j1 
                (if (= numero_jugador 1)
                      (string-append "Mano:\n" (iterar-cartas-cola mano-j1 "" formatear-solo-nombre))
                      (string-append (number->string (length mano-j1)) " cartas en la mano\n")))
              
              (string-mano-j2 
                (if (= numero_jugador 2)
                      (string-append "Mano:\n" (iterar-cartas-cola mano-j2 "" formatear-solo-nombre))
                      (string-append (number->string (length mano-j2)) " cartas en la mano\n")))
              
              ; Construimos strings complejos con recursión de cola
              (string-banca-j1 (iterar-cartas-cola banca-j1 "" formatear-una-carta))
              (string-descarte-j1 (iterar-cartas-cola descarte-j1 "" formatear-solo-nombre))
         
              (string-banca-j2 (iterar-cartas-cola banca-j2 "" formatear-una-carta))
              (string-descarte-j2 (iterar-cartas-cola descarte-j2 "" formatear-solo-nombre))
         
              ; En caso de inicio de juego (sin pokemon activo)
              (string-activo-j1 
                (if (null? activo-j1) 
                      "Sin Pokémon Activo\n" 
                      (formatear-una-carta activo-j1)))

              (string-activo-j2 
                (if (null? activo-j2) 
                      "Sin Pokémon Activo\n" 
                      (formatear-una-carta activo-j2))))
          

              ; Finalmente, se construye el string completo
              (string-append
                      "=== TURNO ACTUAL: " (number->string turno-actual) " ===\n\n"
                      "--- JUGADOR 1 ---\n"
                      string-mano-j1
                      "Mazo: " (number->string (length (get-mazo jugador1))) " cartas disponibles\n"
                      "Premios: " (number->string (length (get-premios jugador1))) " disponibles\n"
                      "Activo:\n" string-activo-j1
                      "Banca:\n" string-banca-j1
                      "Descarte:\n" string-descarte-j1 "\n"
                      "--- JUGADOR 2 ---\n"
                      string-mano-j2
                      "Mazo: " (number->string (length (get-mazo jugador2))) " cartas disponibles\n"
                      "Premios: " (number->string (length (get-premios jugador2))) " disponibles\n"
                      "Activo:\n" string-activo-j2
                      "Banca:\n" string-banca-j2
                      "Descarte:\n" string-descarte-j2 "\n")))


; RF08
; Dom: juego (Juego) X cartaDeLaMano (card)
; Rec: Juego
(define (playToBench juego cartaDeLaMano)
        (let* (; Extraemos los datos del jugador actual
              (turno (get-turno juego))
              (jugador-actual (if (= turno 1) (get-jugador1 juego) (get-jugador2 juego)))
         
              ; Extraemos las listas que vamos a modificar
              (mano-actual (get-mano jugador-actual))
              (banca-actual (get-banca jugador-actual)))

          
              (cond
                    ; Condición 1: La banca está llena (Límite de 5)
                    [(= (length banca-actual) 5)
                    (write "Error: La banca está llena.\n")
                    juego] ; Retornamos el juego, la jugada falla.
       
                    ; Condición 2: Es un Pokémon y no es Básico
                    [(not (and (eq? (get-tipoCarta cartaDeLaMano) 'pokemon) 
                               (null? (get-evo cartaDeLaMano))))
                    (write "Error: Solo puedes bajar Pokémon básicos a la banca.\n")
                    juego] ; Retornamos el juego.
       
                    ; Condición 3: La carta no está en la mano
                    [(not (member cartaDeLaMano mano-actual))
                    (write "Error: La carta no está en la mano del jugador.\n")
                    juego]
       
                    ; CASO DE ÉXITO: Modificamos el estado
                    [else
                          (let* (; Creamos la nueva mano quitando la carta jugada
                                (nueva-mano (remover-primera-ocurrencia cartaDeLaMano mano-actual))
              
                                ; Creamos la nueva banca añadiendo la carta
                                (nueva-banca (append banca-actual (list (init-carta-en-juego cartaDeLaMano (randomPuro turno)))))
              
                                ; Empaquetamos al jugador con sus nuevas listas
                                (nuevo-jugador0 (set-mano jugador-actual nueva-mano))
                                (nuevo-jugador0.1 (set-banca nuevo-jugador0 nueva-banca)))
              
                                ; Empaquetamos el juego final dependiendo de quién era el turno
                                (if (= turno 1)
                                    (init-juego nuevo-jugador0.1 (get-jugador2 juego) turno)
                                    (init-juego (get-jugador1 juego) nuevo-jugador0.1 turno)))])))


; RF09
; Dom: juego (Juego) X cartaDeLaBanca (card)
; Rec: Juego
(define (changeActivePokemon juego cartaBanca)
        (let* (; Extraemos al jugador actual
              (turno (get-turno juego))
              (jugador-actual (if (= turno 1) (get-jugador1 juego) (get-jugador2 juego)))
         
              ; Extraemos listas clave
             (activo-actual (get-activo jugador-actual))
             (banca-actual (get-banca jugador-actual))
             (descarte-actual (get-descarte jugador-actual)))
         
             (cond
                   ; VALIDADOR 1: La carta está realmente en la banca
                   [(not (member cartaBanca banca-actual))
                   (write "Error: El Pokémon elegido no está en la banca.\n")
                   juego]
       
                  ; Caso 1: No hay Pokémon activo
                  [(null? activo-actual)
                  (let* ((nueva-banca (remover-primera-ocurrencia cartaBanca banca-actual))
                        (nuevo-jugador (set-activo (set-banca jugador-actual nueva-banca) cartaBanca)))
                    
                        (if (= turno 1) (set-jugador1 juego nuevo-jugador) (set-jugador2 juego nuevo-jugador)))]
         
                 ; Caso 2: Hay un Pokémon activo
                 [else
                       (let* (; Leemos el coste y las energías disponibles
                             (carta-base-activo (get-carta-base activo-actual))
                             (costo-retirada (get-coste-ret carta-base-activo))
                             (energias-unidas (get-energias activo-actual)))
              
                             (if (< (length energias-unidas) costo-retirada)
                             ; VALIDADOR 2: No tiene las energías suficientes
                             (begin ; Se usa "begin" para que el if no se caiga y ejecute ambas líneas como verdadero
                                (write "Error: No tienes suficientes energías para pagar la retirada.\n")
                                juego)
               
                             ; ÉXITO: Se cobran las energías y se cambia el pokemon activo
                             (let* (; Matemáticas de energías
                                   (energias-a-descartar (tomar-n energias-unidas costo-retirada))
                                   (energias-sobrantes (botar-n energias-unidas costo-retirada))
                    
                                    ; Limpiamos al activo que se retira
                                    (activo-limpio (set-energias (set-estado activo-actual "normal") energias-sobrantes))
                    
                                    ; Reorganizamos el tablero
                                    (nueva-banca (append (remover-primera-ocurrencia cartaBanca banca-actual) 
                                                 (list activo-limpio)))
                                    (nuevo-descarte (append descarte-actual energias-a-descartar))
                    
                                    ; Armamos al jugador y al juego
                                    (nuevo-jugador (init-jugador (get-mazo jugador-actual)
                                                                 (get-mano jugador-actual)
                                                                 cartaBanca   
                                                                 nueva-banca  
                                                                 nuevo-descarte
                                                                 (get-premios jugador-actual)))) 
                                   (if (= turno 1) (set-jugador1 juego nuevo-jugador) (set-jugador2 juego nuevo-jugador)))))])))


; RF10
; Dom: juego (Juego)
; Rec: Juego
(define (drawCardFromDeck juego)
        (let* (; Extraemos los datos necesarios
               (turno (get-turno juego))
               (jugador-actual (if (= turno 1) (get-jugador1 juego) (get-jugador2 juego)))
               (mazo-jugador (get-mazo jugador-actual))
               (mano-actual (get-mano jugador-actual)))

               (cond ; Caso 1: El mazo está vacío
                    [(null? mazo-jugador) 
                     (begin
                           (write "GAME OVER")
                           juego)]
                    ; Caso 2: Saca una carta del mazo
                    [else 
                          (let* (; Robamos carta, actualizamos mazo y mano
                                (carta-robada (car mazo-jugador))
                                (nuevo-mazo (cdr mazo-jugador))
                                (nueva-mano (append mano-actual (list carta-robada)))
                                (nuevo-jugador (set-mano (set-mazo jugador-actual nuevo-mazo) nueva-mano)))

                                ; Actualizamos jugador y juego
                                (if (= turno 1) (set-jugador1 juego nuevo-jugador) (set-jugador2 juego nuevo-jugador)))])))


; RF11
; Dom: juego (Juego) X cartaPokemon (card) X cartaEnergia (card)
; Rec: Juego
(define (useEnergyCard juego cartaPokemon cartaEnergia)
          (let* ((turno (get-turno juego))
                (jugador-actual (if (= turno 1) (get-jugador1 juego) (get-jugador2 juego)))
                (mano-actual (get-mano jugador-actual)))
            
                (cond ; VALIDADOR: La energia debe estar en la mano y la carta debe ser de tipo energy
                     [(or (not (member cartaEnergia mano-actual)) (not (eq? (get-tipoCarta cartaEnergia) 'energy)))
                          (begin
                                 (write "Error: Jugada inválida")
                                 juego)]
                 
                      ; ÉXITO: Se coloca la energía al pokemon
                     [else
                          (let* (; Extraemos datos claves
                                (banca-actual (get-banca jugador-actual))
                                (activo-actual (get-activo jugador-actual))

                                ; Quitamos la energía de la mano
                                (mano-sin-energia (remover-primera-ocurrencia cartaEnergia mano-actual))

                                ; Construimos el pokemon con la energía asociada
                                (poke-con-energia (set-energias cartaPokemon (append (get-energias cartaPokemon) (list cartaEnergia)))))

                                ; Verificamos si el pokemon que vamos a incorporarle la energía es el activo o no
                                (if (equal? cartaPokemon activo-actual)
                                     ; Caso Vedadero: Actualizamos datos
                                     (let* (; Cambiamos al activo y la mano
                                           (jugador-con-energia-activo (set-activo jugador-actual poke-con-energia))
                                           (nuevo-jugador (set-mano jugador-con-energia-activo mano-sin-energia)))

                                           (if (= turno 1) (set-jugador1 juego nuevo-jugador) (set-jugador2 juego nuevo-jugador)))

                                     ; Caso Falso
                                     (let* (; Recorremos la banca. Si encontramos la carta vieja, ponemos la nueva. Si no, dejamos la que estaba.
                                           (nueva-banca (map (lambda (p) (if (equal? p cartaPokemon) poke-con-energia p)) banca-actual))
         
                                           ; Armamos el jugador y el juego
                                           (jugador-con-energia-banca (set-banca jugador-actual nueva-banca))
                                           (nuevo-jugador (set-mano jugador-con-energia-banca mano-sin-energia)))
         
                                           (if (= turno 1) (set-jugador1 juego nuevo-jugador) (set-jugador2 juego nuevo-jugador)))))])))





; RF12
; Dom:
; Rec:
(define (evolvePokemon juego cartaPokemon cartaPokemonEvo)
        (cond ; VALIDADOR: La carta a evolucionar no tiene evolucion o la carta a la que se quiere evolucionar no es compatible
             [(or (null? (get-evo cartaPokemonEvo)) (not (equal? (get-nombre-card (get-carta-base cartaPokemon)) (get-evo cartaPokemonEvo))))
                  (begin
                        (write "Error: Evolución inválida")
                        juego))]
             ; ÉXITO: Se evoluciona el pokemon
             [else
                  (let* (; Extraemos datos claves para la evolución
                         (turno (get-turno juego))
                         (jugador-actual (if (= turno 1) (get-jugador1 juego) (get-jugador2 juego)))
                         (mano-actual (get-mano jugador-actual))
                         (banca-actual (get-banca jugador-actual))
                         (activo-actual (get-activo jugador-actual))

                         ; Quitamos el pokemon evo de la mano
                         (mano-sin-evo (remover-primera-ocurrencia cartaPokemonEvo mano-actual))

                         ; Construimos el pokemon evolucionado
                         (poke-evo (set-carta-base 
                                   (set-cartas-asociadas cartaPokemon (append (get-cartas-asociadas cartaPokemon) (list cartaPokemon))) 
                                   cartaPokemonEvo)))

                         ; Verificamos si el pokemon que vamos a evolucionar es el activo o no
                         (if (equal? cartaPokemon activo-actual)
                             ; Caso Vedadero: Actualizamos datos
                             (let* (; Cambiamos al activo y la mano
                                    (jugador-con-evo-activo (set-activo jugador-actual poke-evo))
                                    (nuevo-jugador (set-mano jugador-con-evo-activo mano-sin-evo)))

                                   (if (= turno 1) (set-jugador1 juego nuevo-jugador) (set-jugador2 juego nuevo-jugador)))

                             ; Caso Falso
                             (let* (; Recorremos la banca. Si encontramos la carta vieja, ponemos la nueva. Si no, dejamos la que estaba.
                                   (nueva-banca (map (lambda (p) (if (equal? p cartaPokemon) poke-evo p)) banca-actual))
         
                                   ; Armamos el jugador y el juego
                                   (jugador-con-evo-banca (set-banca jugador-actual nueva-banca))
                                   (nuevo-jugador (set-mano jugador-con-evo-banca mano-sin-evo)))
         
                                   (if (= turno 1) (set-jugador1 juego nuevo-jugador) (set-jugador2 juego nuevo-jugador)))))]))


; RF13
; Dom: juego (Juego) X cartaEntrenador (Card) X FuncionAdditionalArguments
; Rec: Juego
(define (useTrainerCard juego cartaEntrenador FuncionAdditionalArguments)
          (let* ((turno (get-turno juego))
                 (jugador-actual (if (= turno 1) (get-jugador1 juego) (get-jugador2 juego)))
                 (mano-actual (get-mano jugador-actual))
                 (descarte-actual (get-descarte jugador-actual)))
         
                 (cond
                      ; VALIDADOR: La carta no es de entrenador y no está en la mano
                      [(not (and (eq? (get-tipoCarta cartaEntrenador) 'trainer) 
                            (member cartaEntrenador mano-actual)))
                      (begin
                            (write "Error: Carta de entrenador inválida o no está en la mano.\n")
                            juego)]
         
                [else
                     (let* (; Sacar de la mano la carta de entrenador y la mandamos al descarte
                           (nueva-mano (remover-primera-ocurrencia cartaEntrenador mano-actual))
                           (nuevo-descarte (append descarte-actual (list cartaEntrenador)))
                           (funcion (get-accion cartaEntrenador))
              
                           ; Empaquetamos el jugador y el juego INTERMEDIO
                           (jugador-intermedio (set-descarte (set-mano jugador-actual nueva-mano) nuevo-descarte))
                           (juego-intermedio (if (= turno 1) (set-jugador1 juego jugador-intermedio) (set-jugador2 juego jugador-intermedio))))
              
         ;Por último, ejecutamos la función inyectada sobre el juego intermedio
         (funcion juego-intermedio FuncionAdditionalArguments))])))











