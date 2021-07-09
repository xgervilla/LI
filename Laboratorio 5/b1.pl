main:-
	EstadoInicial = [0,0],
	EstadoFinal = [0,4],
	between(1,1000,CosteMax),
	camino(CosteMax, EstadoInicial, EstadoFinal, [EstadoInicial], Camino),
	reverse(Camino,Camino1), write(Camino1), write(" con coste "), write(CosteMax), nl, halt.

camino(0,E,E,C,C).
camino(CosteMax, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal):-
	CosteMax>0,
	unPaso(CostePaso, EstadoActual, EstadoSiguiente),
	\+member(EstadoSiguiente, CaminoHastaAhora),
	CosteMax1 is CosteMax-CostePaso,
	camino(CosteMax1,EstadoSiguiente,EstadoFinal, [EstadoSiguiente|CaminoHastaAhora], CaminoTotal).

%	[cinco,ocho] -> [cincoNuevo, ochoNuevo]
unPaso(1, [_,O],[5,O]).	%llenamos el de 5
unPaso(1, [C,_],[C,8]).	%llenamos el de 8
unPaso(1, [_,O],[0,O]). %vaciamos el de 5
unPaso(1, [C,_],[C,0]). %vaciamos el de 8
%vaciamos el de 5 en el de 8
unPaso(1, [C,O],[CN,ON]):-
	Dif is min(C,8-O),	%lo maximo que podemos verter (C o lo que quede, 8-O)
	ON is O+Dif,	%llenamos el de 8
	CN is C-Dif,	%quitamos lo que hemos usado en el de 5
	%comprobamos que los dos cubos generados son validos
	ON=<8, CN>=0.
%vaciamos el de 8 en el de 5
unPaso(1, [C,O],[CN,ON]):-
	Dif is min(5,5-C),	%lo maximo que podemos verter (5 o lo que quede, 5-N)
	CN is C+Dif,	%llenamos el de 5
	ON is O-Dif,	%quitamos lo que hemos usado en el de 8
	%comprobamos que los dos cubos generados son validos
	ON>=0, CN=<5.

