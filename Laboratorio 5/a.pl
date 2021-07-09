%casa roja -> peru
%frances -> perro
%chino -> ron
%hungaro -> 1
%verde -> conyac
%verde -> izquierda de la blanca --> blanca -> derecha de la verde ([verde][blanca])
%escultor -> caracoles
%amarilla -> pintor
% 3 -> cava
%vecino del actor -> caballo --> ([actor][caballo] O [caballo][actor])
%hungaro -> vecino de la casa azul --> ([hungaro][azul] O [azul][hungaro])
%notario -> whiskey
%vecino medico -> ardilla --> ([medico][ardilla] O [ardilla][medico])


%numero de casa / color / profesion / animal / bebida / pais
main:- Sol = [[1,_,_,_,_,_],[2,_,_,_,_,_],[3,_,_,_,_,_],[4,_,_,_,_,_],[5,_,_,_,_,_]], llenaSol(Sol),!,halt.

llenaSol(Sol):-member([_,roja,_,_,_,peru], Sol),
	member([_,_,_,perro,_,francia], Sol),
	member([_,_,_,_,ron,chino], Sol),
	member([1,_,_,_,_,hungaro], Sol),
	member([_,verde,_,_,conyac,_], Sol),
	member([_,_,escultor,caracoles,_,_], Sol),
	member([_,amarilla,pintor,_,_,_], Sol),
	member([3,_,_,_,cava,_], Sol),
	member([2,azul,_,_,_,_], Sol),
	member([_,_,notario,_,whiskey,_], Sol),
	member([N,blanca,_,_,_,_], Sol), member([N1,verde,_,_,_,_], Sol), N is N1+1,
	%el que vive al lado del actor tiene un caballo Y el que vive al lado del medico tiene una ardilla -> ambos se refieren a los extremos
	member([N2,_,actor,_,_,_], Sol), member([N3,_,_,caballo,_,_], Sol), N3 is N2+1,
	member([N4,_,medico,_,_,_], Sol), member([N5,_,_,ardilla,_,_], Sol), N4 is N5+1,
	writeSol(Sol),nl,nl. %fail. -> descomentar para ver otras opciones

writeSol([]).
writeSol([L1|L]):- write(L1), nl, writeSol(L),!.
