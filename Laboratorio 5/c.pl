p:- R = [R1, R2, R3], A = [A1, A2, A3], V = [V1, V2, V3],
	permutation([1,2,3,4,5,6,7,8,9], [R1,R2,R3,A1,A2,A3,V1,V2,V3]),
	gana(R,A), gana(A,V), gana(V,R), write(R), write("  "), write(A), write("  "), write(V), write("  "),!.%,nl,fail,!. --> descomentar para ver todas las posibles combinaciones (sin evitar repeticiones)

%D1 en al menos 5 de las 9 combinaciones

gana(D1,D2):- findall(_, (member(D1sub,D1), member(D2sub,D2), D1sub>D2sub), L), length(L,X), X>=5.

%R1-A1, R1-A2, R1-A3
%R2-A1, R2-A2, R2-A3
%R3-A1, R3-A2, R3-A3 --> de los 9 "combates" debe ganar 5 para ser mejor dado