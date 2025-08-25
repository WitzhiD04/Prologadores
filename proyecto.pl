%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROYECTO: Misioneros y Caníbales — Búsqueda en Amplitud Limitada (BFS limitada)
% Lenguaje: Prolog
%
% OBJETIVO DEL PROYECTO
% ---------------------
% Resolver el problema clásico de los misioneros y caníbales aplicando un
% método de búsqueda "a ciegas" basado en amplitud, con la variante
% "Amplitud Limitada": cada vez que se expande un nodo del árbol de búsqueda,
% SOLO se generan (y encolan) los DOS primeros hijos válidos. Si el nodo
% tiene más hijos potenciales, el nodo NO se cierra: se reencola con un
% "índice" que recuerda desde cuál movimiento continuar más adelante.
%
% REPRESENTACIÓN
% --------------
% - Estado: miscan(MI, CI, MD, CD, LadoBote)
%     MI: misioneros en la orilla izquierda
%     CI: caníbales  en la orilla izquierda
%     MD: misioneros en la orilla derecha
%     CD: caníbales  en la orilla derecha
%     LadoBote: 'izquierda' o 'derecha'
%
% - Acción: bote(Bm, Bc, NuevoLado)
%     Bm: misioneros que viajan en el bote
%     Bc: caníbales  que viajan en el bote
%     NuevoLado: lado al que llega el bote ('izquierda'/'derecha')
%
% - Cola de BFS:
%     Lista de elementos de la forma [Camino, Estado, Indice],
%     donde:
%       Camino = lista de acciones aplicadas (última acción al frente)
%       Estado = estado alcanzado tras Camino
%       Indice = posición desde la que debemos retomar la enumeración de
%                posibles movimientos para ESTE estado (para respetar la
%                "Amplitud Limitada" de 2 hijos por expansión).
%
% INVARIANTES Y RESTRICCIONES
% ---------------------------
% - El bote transporta 1 o 2 personas.
% - En ninguna orilla puede haber más caníbales que misioneros si hay al menos
%   un misionero en esa orilla (seguridad).
% - Usamos una lista de 'Visitados' para evitar re-explorar estados repetidos.
%
% SALIDA
% ------
% - Predicado solucion/1 retorna una lista de ACCIONES (bote/3) desde el estado
%   inicial hasta el final, en el orden correcto de ejecución.
%
% CÓMO EJECUTAR
% ?- solucion(S).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1) ESTADO INICIAL Y FINAL  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Todos a la izquierda, bote a la izquierda.
inicial(miscan(3,3,0,0,izquierda)).

% Todos a la derecha, bote a la derecha.
final(miscan(0,0,3,3,derecha)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2) MOVIMIENTOS POSIBLES (CATÁLOGO DE PASAJEROS DEL BOTE) %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Esta lista fija define el orden en el que intentaremos generar
% los hijos de cada estado. Ese orden influye en la forma de la
% solución encontrada, pero NO en la corrección del algoritmo.
% (Bm, Bc) = (misioneros, caníbales) en el bote.
posibles_movimientos([(1,0), (0,1), (2,0), (0,2), (1,1)]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3) TRANSICIÓN: APLICAR UN MOVIMIENTO Y VALIDAR EL RESULTADO %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% siguiente_estado(+Estado, +AccionBote, -EstadoNuevo)
% Dado un 'Estado' y una 'AccionBote = bote(Bm,Bc,NuevoLado)', calcula
% el 'EstadoNuevo' si el movimiento es factible y cumple las restricciones
% (no números negativos, no exceder 3, y seguridad en ambas orillas).

siguiente_estado(
    miscan(MI, CI, MD, CD, Lado),
    bote(Bm, Bc, NuevoLado),
    miscan(NMI, NCI, NMD, NCD, NuevoLado)
) :-
    % Determinar el otro lado al que se mueve el bote.
    otro_lado(Lado, NuevoLado),

    % Dos casos: mover de izquierda->derecha o de derecha->izquierda.
    ( Lado = izquierda ->
        % Requisitos: tener suficientes personas del lado izquierdo.
        MI >= Bm, CI >= Bc,
        % Capacidad del bote: viajar 1 o 2 personas.
        Bm + Bc >= 1, Bm + Bc =< 2,
        % Actualizar conteos: salen de la izquierda, llegan a la derecha.
        NMI is MI - Bm, NCI is CI - Bc,
        NMD is MD + Bm, NCD is CD + Bc
    ; Lado = derecha ->
        % Requisitos: tener suficientes personas del lado derecho.
        MD >= Bm, CD >= Bc,
        % Capacidad del bote: 1 o 2 personas.
        Bm + Bc >= 1, Bm + Bc =< 2,
        % Actualizar conteos: salen de la derecha, llegan a la izquierda.
        NMD is MD - Bm, NCD is CD - Bc,
        NMI is MI + Bm, NCI is CI + Bc
    ),

    % Cotas: no negativos y no más de 3 por orilla.
    NMI >= 0, NCI >= 0, NMD >= 0, NCD >= 0,
    NMI =< 3, NCI =< 3, NMD =< 3, NCD =< 3,

    % Seguridad: en cada orilla, si hay misioneros, no pueden ser superados
    % por caníbales.
    (NMI = 0 ; NMI >= NCI),
    (NMD = 0 ; NMD >= NCD).


% otro_lado(+Lado, -Otro)
% True si 'Otro' es el lado contrario a 'Lado'.
otro_lado(izquierda, derecha).
otro_lado(derecha, izquierda).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4) PREDICADO PRINCIPAL: BUSCAR LA SOLUCIÓN (BFS LIM) %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solucion(-Solucion)
% Devuelve la lista de acciones (bote/3) que llevan del estado inicial
% al estado final, respetando la "Amplitud Limitada".
%
% Estructura inicial de la cola:
%   [[ [], Inicial, 0 ]]
%   - Camino = [] (aún no hay acciones aplicadas)
%   - Estado = Inicial
%   - Indice = 0 (aún no intentamos ningún movimiento de 'posibles_movimientos')

solucion(Solucion) :-
    inicial(Inicial),
    posibles_movimientos(Movs),
    bfs([[ [], Inicial, 0 ]], [Inicial], Movs, Solucion).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 5) BÚSQUEDA EN AMPLITUD LIMITADA (BFS LIMITADA)   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% bfs(+Cola, +Visitados, +Movs, -Solucion)
%
% - Cola: lista de entradas [Camino, Estado, Indice].
% - Visitados: conjunto (lista) de estados ya visitados globalmente.
% - Movs: catálogo fijo de movimientos posibles (en orden).
% - Solucion: lista de acciones (bote/3) desde el inicio hasta el final.

% Caso base: la primera entrada de la cola ya está en el estado final.
% Devolvemos el Camino en el orden correcto (está invertido).
bfs([[Camino, Final, _] | _], _, _, Solucion) :-
    final(Final),
    reverse(Camino, Solucion).

% Caso general:
% - Expandimos EL PRIMER elemento de la cola (BFS).
% - Generamos HASTA 2 hijos válidos (limitación).
% - Encolamos esos hijos al final (orden FIFO).
% - Si el estado actual tiene más movimientos que no alcanzamos a probar,
%   reencolamos ESTE MISMO NODO con 'Indice' avanzado para probarlos más tarde.
bfs([ [Camino, Estado, Indice] | Cola], Visitados, Movs, Solucion) :-

    % Genera hasta 2 hijos válidos empezando desde 'Indice' en 'Movs'.
    generar_hijos_limitado(Estado, Indice, Movs, 0, NuevosHijos, NuevoIndice),

    % Agrega a la cola esos hijos (si no están en Visitados),
    % actualizando la lista de Visitados en el proceso.
    agregar_a_cola(NuevosHijos, Camino, Visitados, Cola, NuevaCola, NuevosVisitados),

    % Si 'NuevoIndice' todavía no alcanza el total de movimientos posibles,
    % significa que quedaron movimientos por probar; por lo tanto,
    % reencolamos el MISMO NODO con 'Indice = NuevoIndice'.
    length(Movs, LenMovs),
    ( NuevoIndice < LenMovs ->
        append(NuevaCola, [[Camino, Estado, NuevoIndice]], ColaFinal)
    ; ColaFinal = NuevaCola
    ),

    % Continuamos el BFS con la cola actualizada.
    bfs(ColaFinal, NuevosVisitados, Movs, Solucion).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 6) GENERADOR LIMITADO DE HIGOS (MÁXIMO 2 POR EXPANSIÓN)   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generar_hijos_limitado(+Estado, +Indice, +Movs, +Cont,
%                        -Hijos,  -IndiceFinal)
%
% - Recorre la lista 'Movs' desde 'Indice', intentando construir sucesores
%   con 'siguiente_estado/3'.
% - Se detiene cuando:
%     a) se han generado 2 hijos válidos (Cont = 2), o
%     b) se han agotado los movimientos (Indice llega al final de 'Movs').
% - Devuelve:
%     Hijos = lista de pares (Accion, NuevoEstado)
%     IndiceFinal = siguiente índice desde el cual retomar en una futura expansión.

% Corte: sin más movimientos o ya generamos 2 hijos.
generar_hijos_limitado(_, Indice, Movs, Cont, [], Indice) :-
    length(Movs, LenMovs),
    (Indice >= LenMovs ; Cont >= 2).

% Caso recursivo: intentamos con el movimiento en 'Indice'.
generar_hijos_limitado(Estado, Indice, Movs, Cont,
                       [ (Accion, NuevoEstado) | Resto ],
                       IndFin) :-
    nth0(Indice, Movs, (Bm, Bc)),              % Tomar el movimiento (Bm,Bc)
    Estado = miscan(_,_,_,_,Lado),
    otro_lado(Lado, NuevoLado),
    Accion = bote(Bm, Bc, NuevoLado),          % Construir la acción 'bote/3'

    % Si el movimiento produce un estado válido, lo contamos (Cont+1).
    ( siguiente_estado(Estado, Accion, NuevoEstado) ->
        NuevoCont is Cont + 1,
        SigInd   is Indice + 1,
        generar_hijos_limitado(Estado, SigInd, Movs, NuevoCont, Resto, IndFin)
    ; % Movimiento inválido: NO cuenta como hijo, solo avanzamos el índice.
        SigInd is Indice + 1,
        generar_hijos_limitado(Estado, SigInd, Movs, Cont, Resto, IndFin)
    ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 7) ENCOLAR HIJOS (RESPETANDO VISITADOS) Y ACTUALIZAR FIFO %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% agregar_a_cola(+Hijos, +Camino, +Visitados,
%                +ColaIn, -ColaOut, -VisitadosOut)
%
% - Recorre la lista de hijos (Accion, NuevoEstado).
% - Si 'NuevoEstado' ya fue visitado, se descarta.
% - Si no, se construye un NUEVO camino agregando 'Accion' al frente,
%   se encola [NuevoCamino, NuevoEstado, 0] al final de la Cola,
%   y se añade 'NuevoEstado' a Visitados.

agregar_a_cola([], _, Visitados, Cola, Cola, Visitados).

agregar_a_cola([(Accion, NuevoEstado) | Resto],
               Camino, Visitados, Cola, NuevaCola, NuevosVisitados) :-

    ( member(NuevoEstado, Visitados) ->
        % Ya visitado: lo ignoramos y seguimos.
        agregar_a_cola(Resto, Camino, Visitados, Cola, NuevaCola, NuevosVisitados)
    ;
        % No visitado: construimos el nuevo nodo de la cola.
        NuevoCamino = [Accion | Camino],
        append(Cola, [[NuevoCamino, NuevoEstado, 0]], ColaTemp),
        agregar_a_cola(Resto, Camino, [NuevoEstado | Visitados],
                       ColaTemp, NuevaCola, NuevosVisitados)
    ).