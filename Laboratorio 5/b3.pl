main:-
	%Costes = [1,2,5,8],
	EstadoInicial = [[1,2,5,8],0],
	EstadoFinal = [[],1],
	between(1,1000,CosteMax),
	camino(CosteMax, EstadoInicial, EstadoFinal, [EstadoInicial], Camino),
	reverse(Camino,Camino1), write(Camino1), write(" con coste "), write(CosteMax), nl,!.

camino(0,E,E,C,C).
camino(CosteMax, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal):-
	%CosteMax>0,
	unPaso(CostePaso, EstadoActual, EstadoSiguiente),
	\+member(EstadoSiguiente, CaminoHastaAhora),
	CosteMax1 is CosteMax-CostePaso,
	CosteMax1>=0,
	camino(CosteMax1,EstadoSiguiente,EstadoFinal, [EstadoSiguiente|CaminoHastaAhora], CaminoTotal).


%de izquierda a derecha ->

%cruza una persona
%selecciona el tiempo de L1 y "quita", esa persona pasa a estar en el otro lado
unPaso(Tiempo,[L1,0],[L2,1]):- select(Tiempo, L1, L2).

%cruzan dos personas
%cogemos dos tiempos de L1 (el resultado final queda en l2) y el tiempo es el maximo
unPaso(Tiempo,[L1,0],[L2,1]):- select(T1, L1, Laux), select(T2, Laux, L2), Tiempo is max(T1,T2).

% de derecha a izquierda <-

%cruza una persona
%cogemos un tiempo que sea valido y NO pertenezca a L1, ordenamos la nueva L1 (junto con la persona que cruza para generar el nuevo lado)
unPaso(Tiempo,[L1,1],[L2,0]):- member(Tiempo,[1,2,5,8]), not(member(Tiempo,L1)),
	sort([Tiempo|L1], L2).

%cruzan dos personas
unPaso(Tiempo,[L1,1],[L2,0]):- member(T1,[1,2,5,8]), not(member(T1,L1)),
	member(T2,[1,2,5,8]), not(member(T2,L1)), T1\=T2, Tiempo is max(T1,T2),
	sort([T1,T2|L1], L2).









%cogemos una persona
%member(P, [C1,C2,C3,C4]),
%cogemos su coste (nth(index,lista,elem))
%nth0(Index, [C1,C2,C3,C4], P),
%cogemos el coste de cruzar y lo asignamos
%nth0(Index, Costes, C),
%actualizamos el estado