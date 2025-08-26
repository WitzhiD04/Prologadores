%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROYECTO: Misioneros y Caníbales — Búsqueda en Amplitud Limitada (BFS limitada)
% Lenguaje: Prolog
% 
% Carlos Daniel Guiza, Samuel Gantiva, Juan Felipe Rubiano
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
% - Estado: inicial(jarra(J4,J3)).
%			J4: jarra de 4 litros
%			J3: jarra de 3 litros
% - Acción: 
%			llenar(jx): llena la jarra x a su máxima capacidad
%			vaciar(jx): vacía la jarra x
%			traspasar(jx,jy): pasa lo que está en jx a jy teniendo en cuenta la capacidad máxima de jy.
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
% 
% - En J4 no puede haber más de 4 litros, y en J3 no puede haber más de 3 litros.
% - Se llena a la máxima capacidad, y se vacía a 0, no se puede vaciar o llenar parcialmente.
% - Usamos una lista de 'Visitados' para evitar re-explorar estados repetidos.
%
% SALIDA
% ------
% - Predicado solucion/1 retorna una lista de ACCIONES desde el estado
%   inicial hasta el final, en el orden correcto de ejecución.
%
% CÓMO EJECUTAR
% ?- solucion(S).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1) ESTADO INICIAL Y FINAL  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


inicial(jarra(0,0)).


final(jarra(2,_)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2) MOVIMIENTOS POSIBLES (CATÁLOGO DE PASAJEROS DEL BOTE) %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Esta lista fija define el orden en el que intentaremos generar
% los hijos de cada estado. Ese orden influye en la forma de la
% solución encontrada, pero NO en la corrección del algoritmo.
posibles_movimientos([
			llenar(j4),
            llenar(j3),
            vaciar(j4),
            vaciar(j3),
            traspasar(j4,j3),
            traspasar(j3,j4)
]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3) TRANSICIÓN: APLICAR UN MOVIMIENTO Y VALIDAR EL RESULTADO %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% siguiente_estado(+Estado, +Accion, -EstadoNuevo)
% Dado un 'Estado' y una 'Accion', calcula
% el 'EstadoNuevo' si el movimiento es factible y cumple las restricciones
% (no números negativos, no exceder 3 para J3, no exceder 4 para J4.

siguiente_estado(jarra(J4,J3), llenar(j4), jarra(4,J3)) :- J4 < 4.
siguiente_estado(jarra(J4,J3), llenar(j3), jarra(J4,3)) :- J3 < 3.

siguiente_estado(jarra(J4,J3), vaciar(j4), jarra(0,J3)) :- J4 > 0.
siguiente_estado(jarra(J4,J3), vaciar(j3), jarra(J4,0)) :- J3 > 0.

siguiente_estado(jarra(J4,J3), traspasar(j4,j3), jarra(NJ4,NJ3)) :- 
		J4 > 0,
    	J3 < 3,
    	NJ4 is J4 - min(J4,3-J3),
    	NJ3 is J3 + min(J4,3-J3).

siguiente_estado(jarra(J4,J3), traspasar(j3,j4), jarra(NJ4,NJ3)) :- 
		J3 > 0,
    	J4 < 4,
    	NJ3 is J3 - min(J3,4-J4),
    	NJ4 is J4 + min(J3,4-J4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4) PREDICADO PRINCIPAL: BUSCAR LA SOLUCIÓN (BFS LIM) %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solucion(-Solucion)
% Devuelve la lista de acciones que llevan del estado inicial
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
% - Solucion: lista de acciones desde el inicio hasta el final.

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
% 6) GENERADOR LIMITADO DE HIJOS (MÁXIMO 2 POR EXPANSIÓN)   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generar_hijos_limitado(+Estado, +Indice, +Movs, +Cont,
%                        -Hijos,  -IndiceFinal)
%
% - Recorre la lista 'Movs' desde 'Indice', intentando construir sucesores
%   con 'siguiente_estado'.
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
    nth0(Indice, Movs, Accion),              % Tomar el movimiento (Accion)

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