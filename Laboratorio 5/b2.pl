main:-
	EstadoInicial = [3,3,0,0,0],
	EstadoFinal = [0,0,3,3,1],
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


%pasamos 1 o 2 misioneros al lado derecho
unPaso(1, [MLeft, CLeft, MRight, CRight, 0], [MLeftN, CLeft, MRightN, CRight, 1]):-
	member(Tripulantes, [1,2]),	%1 o 2 en la canoa
	MLeftN is MLeft-Tripulantes,
	MRightN is MRight+Tripulantes,
	aSalvo(MLeftN, CLeft, MRightN, CRight).

%pasamos 1 o 2 misioneros al lado izquierdo
unPaso(1, [MLeft, CLeft, MRight, CRight, 1], [MLeftN, CLeft, MRightN, CRight, 0]):-
	member(Tripulantes, [1,2]),	%1 o 2 en la canoa
	MLeftN is MLeft+Tripulantes,
	MRightN is MRight-Tripulantes,
	aSalvo(MLeftN, CLeft, MRightN, CRight).


%pasamos 1 o 2 canibales al lado derecho
unPaso(1, [MLeft, CLeft, MRight, CRight, 0], [MLeft, CLeftN, MRight, CRightN, 1]):-
	member(Tripulantes, [1,2]),	%1 o 2 en la canoa
	CLeftN is CLeft-Tripulantes,
	CRightN is CRight+Tripulantes,
	aSalvo(MLeft, CLeftN, MRight, CRightN).

%pasamos 1 o 2 canibales al lado izquierdo
unPaso(1, [MLeft, CLeft, MRight, CRight, 1], [MLeft, CLeftN, MRight, CRightN, 0]):-
	member(Tripulantes, [1,2]),	%1 o 2 en la canoa
	CLeftN is CLeft+Tripulantes,
	CRightN is CRight-Tripulantes,
	aSalvo(MLeft, CLeftN, MRight, CRightN).


%pasamos 1 de cada al lado derecho
unPaso(1, [MLeft, CLeft, MRight, CRight, 0], [MLeftN, CLeftN, MRightN, CRightN, 1]):-
	CLeftN is CLeft-1, CRightN is CRight+1,
	MLeftN is MLeft-1, MRightN is MRight+1,
	aSalvo(MLeftN, CLeftN, MRightN, CRightN).

%pasamos 1 de cada al lado izquierdo
unPaso(1, [MLeft, CLeft, MRight, CRight, 1], [MLeftN, CLeftN, MRightN, CRightN, 0]):-
	CLeftN is CLeft+1, CRightN is CRight-1,
	MLeftN is MLeft+1, MRightN is MRight-1,
	aSalvo(MLeftN, CLeftN, MRightN, CRightN).


%between(0,3,M), between(0,3,C), between(0,3,MLeft), between(0,3,CLeft)
%MRight=<3, MRight>=0, MLeft=<3, MLeft>=0, CRight=<3, CRight>=0, CLeft=<3, CLeft>=0,
aSalvo(MLeft,CLeft,MRight,CRight):- between(0,3,MRight), between(0,3,CRight), between(0,3,MLeft), between(0,3,CLeft), noPeligro(MRight,CRight), noPeligro(MLeft,CLeft),!.

noPeligro(0,_).
noPeligro(M,C):- M>=C.