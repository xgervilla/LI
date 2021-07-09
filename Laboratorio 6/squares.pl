:- use_module(library(clpfd)).

reload:-[squares].
%ejemplo(_, Big, [S1...SN]): how to fit all squares of sizes S1...SN in a square of size Big?
ejemplo(0,  3,[2,1,1,1,1,1]).
ejemplo(1,  4,[2,2,2,1,1,1,1]).
ejemplo(2,  5,[3,2,2,2,1,1,1,1]).
ejemplo(3, 19,[10,9,7,6,4,4,3,3,3,3,3,2,2,2,1,1,1,1,1,1]).
ejemplo(4, 40,[24,16,16,10,9,8,8,7,7,6,6,3,3,3,2,1,1]).   %<-- aquest ja costa bastant de resoldre...

%Aquests dos ultims son molt durs!!! Si no et surten no et preocupis gaire....:
ejemplo(5,112,[50,42,37,35,33,29,27,25,24,19,18,17,16,15,11,9,8,7,6,4,2]).
ejemplo(6,175,[81,64,56,55,51,43,39,38,35,33,31,30,29,20,18,16,14,9,8,5,4,3,2,1]).


%% Possible output solution for example 3:
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  9  9  9  9  9  9  9  9  9
%%  10 10 10 10 10 10 10 10 10 10  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  2  2
%%   6  6  6  6  6  6  4  4  4  4  7  7  7  7  7  7  7  1  1
%%   3  3  3  3  3  3  4  4  4  4  3  3  3  3  3  3  3  3  3
%%   3  3  3  3  3  3  4  4  4  4  3  3  3  3  3  3  3  3  3
%%   3  3  3  3  3  3  1  1  1  1  3  3  3  3  3  3  3  3  3


main:- 
    ejemplo(2,Big,Sides),
    nl, write('Fitting all squares of size '), write(Sides), write(' into big square of size '), write(Big), nl,nl,
    length(Sides,N), %N sub cuadrados
    length(RowVars,N),  %fila en la que empieza el cuadrado
    length(ColVars,N),  %columna en la que empieza el cuadrado

    %domain
    insideBigSquare(N,Big,Sides,RowVars),
    insideBigSquare(N,Big,Sides,ColVars),

    %constraints
    nonoverlapping(N,Sides,RowVars,ColVars),
    
    %labeling e impresion
    %label(RowVars), label(ColVars),
    labeling([ff],RowVars),% labeling([ff],ColVars),
    displaySol(Big,Sides,RowVars,ColVars), halt.


displaySol(N,Sides,RowVars,ColVars):- 
    between(1,N,Row), nl, between(1,N,Col),
    nth1(K,Sides,S),    
    nth1(K,RowVars,RV),    RVS is RV+S-1,     between(RV,RVS,Row),
    nth1(K,ColVars,CV),    CVS is CV+S-1,     between(CV,CVS,Col),
    writeSide(S), fail.
displaySol(_,_,_,_):- nl,nl,!.

writeSide(S):- S<10, write('  '),write(S),!.
writeSide(S):-       write(' ' ),write(S),!.


%Numero de cuadrados, tamanyo del cuadrado grande, sub cuadrados, variables para identificar el subcuadrado
insideBigSquare(_,_,[],[]):-!.
insideBigSquare(N,Big,[S|Sides],[V|Vars]):-
    %valor de V: entre 1 y Big-tamanyo del sub cuadrado (+1 para normalizar)
    Limite is Big-S+1, V in 1..Limite, N2 is N-1,
    insideBigSquare(N2,Big,Sides,Vars).

nonoverlapping(_,[],[],[]):-!.
nonoverlapping(_,[S|Sides],[R|RowVars],[C|ColVars]):- checkSquare(S,R,C,Sides,RowVars,ColVars), nonoverlapping(_, Sides, RowVars, ColVars).


%comprueba con un cuadrado especifico
checkSquare(_,_,_,[],[],[]):-!.
checkSquare(S1,R1,C1,[S2|Sides], [R2|RowVars], [C2|ColVars]):- noColision(S1,R1,C1,S2,R2,C2), checkSquare(S1,R1,C1,Sides,RowVars,ColVars).


%indica que el cuadrado que empieza en (R1,C1) de tamanyo Size no colisiona con el de (R2,C2)
noColision(Size1, R1, C1, Size2, R2, C2):-
    R1+Size1 #=<R2;
    C1+Size1 #=<C2;
    R2+Size2 #=<R1;
    C2+Size2 #=<C1.






