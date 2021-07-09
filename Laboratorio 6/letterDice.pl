:- use_module(library(clpfd)).

reload:- [letterDice].
c:-write('\e[H\e[2J').

%% A (6-sided) "letter dice" has on each side a different letter.
%% Find four of them, with the 24 letters abcdefghijklmnoprstuvwxy such
%% that you can make all the following words: bake, onyx, echo, oval,
%% gird, smug, jump, torn, luck, viny, lush, wrap.

%Some helpful predicates:

word( [b,a,k,e] ).
word( [o,n,y,x] ).
word( [e,c,h,o] ).
word( [o,v,a,l] ).
word( [g,i,r,d] ).
word( [s,m,u,g] ).
word( [j,u,m,p] ).
word( [t,o,r,n] ).
word( [l,u,c,k] ).
word( [v,i,n,y] ).
word( [l,u,s,h] ).
word( [w,r,a,p] ).

num(X,N):- nth1( N, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,v,w,x,y], X ).

main:-
    length(D1,6),
    length(D2,6),
    length(D3,6),
    length(D4,6),

    %juntamos para tener todas las variables juntas
    append(D1,D2,D12),
    append(D12,D3,D123),
    append(D123,D4,DAll),
    DAll ins 1..24,
    all_distinct(DAll),


    %restricciones:
    %lista de pares de letras que no deben ir juntas, sort para evitar repeticiones
    findall(L1-L2, noColisiona(L1,L2), Words), sort(Words, W),

    %asignamos las constraints para que los dados esten ordenados y para que no contengan dos letras que deben ir separadas
    declareConstraints(D1,D2,D3,D4,W),

    %optimizaciones:
    %por BAKE, b y a pertenecen a diferentes dados (aribtrariamente el 2 y el 1)
    D1 = [1|_],
    D2 = [2|_],

    %establecemos orden entre el tercer y el cuarto dado -> la primera letra del tercero debe ser menor que la primera del cuarto
    D3 = [D3p|_],
    D4 = [D4p|_], D3p #< D4p,
    
    %rellenamos los dados
    labeling([ff],DAll),

    %escribimos la respuesta
    writeN(D1), writeN(D2), writeN(D3), writeN(D4),!. %halt.
    
writeN(D):- findall(X,(member(N,D),num(X,N)),L), write(L), nl, !.

%noColisiona: dada una palabra y dos letras que pertenecen a ella devolvemos el indice de las dos letras
noColisiona(N,M):- word(W), member(L1,W), member(L2,W), num(L1,N), num(L2,M), N<M.

%nos encargamos de que cada dado este ordenado y de que en cada dado no haya letras que deben ir por separado
declareConstraints(D1,D2,D3,D4,L):-
    ordenado(D1),ordenado(D2),ordenado(D3),ordenado(D4), checkLetras(D1,L),
    checkLetras(D2,L), checkLetras(D3,L), checkLetras(D4,L).

%todas las caras del dado estan ordenadas -> evitamos simetrias
ordenado([D1,D2,D3,D4,D5,D6]):- D1#<D2, D2#<D3, D3#<D4, D4#<D5, D5#<D6,!.


%por cada par de Ds, no puede darse que Di = L1 y Dj = L2 -> union de las negaciones
checkLetras(_,[]).
checkLetras([D1,D2,D3,D4,D5,D6], [L1-L2|L]):-
    checkLetras([D1,D2,D3,D4,D5,D6],L),
    D1 #\= L1 #\/ D2 #\= L2,    %si D1 == L1, D2 != L2 y al reves
    D1 #\= L1 #\/ D3 #\= L2,    %si D1 == L1, D3 != L2 y al reves
    D1 #\= L1 #\/ D4 #\= L2,    % ...
    D1 #\= L1 #\/ D5 #\= L2,
    D1 #\= L1 #\/ D6 #\= L2,
    D2 #\= L1 #\/ D3 #\= L2,
    D2 #\= L1 #\/ D4 #\= L2,
    D2 #\= L1 #\/ D5 #\= L2,
    D2 #\= L1 #\/ D6 #\= L2,
    D3 #\= L1 #\/ D4 #\= L2,
    D3 #\= L1 #\/ D5 #\= L2,
    D3 #\= L1 #\/ D6 #\= L2,
    D4 #\= L1 #\/ D5 #\= L2,
    D4 #\= L1 #\/ D6 #\= L2,
    D5 #\= L1 #\/ D6 #\= L2. %llamada recursiva con el resto de letras




%%version inicial (no funciona treatWords)%%

%% %todos los dados deben tener numeros (letras) distintos
%% allTogether([D11,D12,D13,D14,D15,D16], [D21,D22,D23,D24,D25,D26], [D31,D32,D33,D34,D35,D36], [D41,D42,D43,D44,D45,D46]):-
%%     all_different([
%%         D11,D12,D13,D14,D15,D16,
%%         D21,D22,D23,D24,D25,D26,
%%         D31,D32,D33,D34,D35,D36,
%%         D41,D42,D43,D44,D45,D46]).

%% %en todas las palabras NO puede haber dos letras que sean del mismo dado -> TODAS las letras deben de ser de diferentes dados (en una palabra)
%% treatWords(D1,D2,D3,D4):- word(W),write(W),
%%     treatWord(D1,D2,D3,D4,W),fail,!.

%% treatWord(_,_,_,_,[]):-!.
%% treatWord(D1,D2,D3,D4,[L1,L2,L3,L4]):- num(L1,N1), num(L2,N2), num(L3,N3),
%%     num(L4,N4), permutation([N1,N2,N3,N4],[E1,E2,E3,E4]),
%%     %dados los numeros de las letras que representan, buscamos la permutacion tal que cada una de las 4 letras pertenece a un solo dado
%%     element(_,D1,E1), element(_,D2,E2), element(_,D3,E3), element(_,D4,E4),
%%     write([E1,E2,E3,E4]),nl,!.