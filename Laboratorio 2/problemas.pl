padre(juan,pedro).
padre(maria,pedro).
hermano(pedro,vicente).
hermano(pedro,alberto).

%tio:
tio(X,Y):- padre(X,Z), hermano(Z,Y).

%generacion de numeros naturales
nat(0).
nat(N):- nat(N1), N is N1+1.

%escribir todo directamente:
%nat(N), write(N),nl,fail.

%minimo comun multiplo:
mcm(X,Y,M):- nat(M), M>0, 0 is M mod X, 0 is M mod Y, !.    %para mas mcm quitar el corte
%mas eficiente:
%mcm(X,Y,M):- nat(N), N>0, M is N * X, 0 is M mod Y.

%factorial:
fact(0,1):-!.
fact(X,F):- X1 is X-1, fact(X1,F1), F is F1 * X.

%listas:

%X pertenece a una lista == member(X,Lis)
pertenece(X, [X|_]).
pertenece(X, [_|L]):- pertenece(X,L).

%X pertenece a una lista y deja el resto de la lista en Resto
pertConResto(X,L,Resto):- append(L1, [X|L2],L), append(L1,L2,Resto).

%concatenar dos listas en L3 == append(L1,L2,L3)
concatenar([],L,L).
concatenar([X|L1], L2, [X|L3]):- concatenar(L1,L2,L3).

%permutacion(L,P) -> P es una permutacion de L
permutacion([],[]).
permutacion([X|L], P):- permutacion(L,P1), concatenar(Pa,Pb,P1), concatenar(Pa,[X|Pb],P).

%subconjuntos de L (2^n)
subconjunto([],[]).
subconjunto([_|L],S):- subconjunto(L,S).
subconjunto([X|L],[X|S]):- subconjunto(L,S).

%longitud de una lista:
long([],0).
long([_|L],N):- long(L,N1), N is N1+1.


%factores primos
factoresPrimos(1,[]).
factoresPrimos(N,[F|L]):- nat(F), F>1, 0 is N mod F, N1 is N // F, factoresPrimos(N1,L),!.


%derivada:
der( X, X, 1):- !.	%derivada de x = 1
der( C, _, 0):- atomic(C).
der( A+B, X, U+V ):- der(A,X,U), der(B,X,V). 
der( A*B, X, A*V+B*U ):- der(A,X,U), der(B,X,V).
der(A-B,X,DA-DB):- der(A,X,DA),der(B,X,DB).
der(sin(A),X,cos(A)*DA):-der(A,X,DA).
der(cos(A),X,-sin(A)*DA):- der(A,X,DA).
der(e^A,X,DA*e^A):- der(A,X,DA).
der(ln(A),X,DA*1/A):- der(A,X,DA).

simplifica(E,E1):- unpaso(E,E2),!, simplifica(E2,E1).
simplifica(E,E).

unpaso(A+B,A+C):- unpaso(B,C),!.
unpaso(B+A,C+A):- unpaso(B,C),!.
unpaso(A*B,A*C):- unpaso(B,C),!.
unpaso(B*A,C*A):- unpaso(B,C),!.
unpaso(0*_,0):-!.
unpaso(_*0,0):-!.
unpaso(1*X,X):-!.
unpaso(X*1,X):-!.
unpaso(0+X,X):-!.
unpaso(X+0,X):-!.
unpaso(N1+N2,N3):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*N2,N3):- number(N1), number(N2), N3 is N1*N2,!.
unpaso(N1*X+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*X+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!. 


%cifras:
cifras(L,N):- subconjunto(L,S), permutacion(S,P), expresion(P,E), N is E, write(E), nl, fail.
%cojemos un subconjunto de L y lo permutamos. Una vez permutado le aplicamos las expresiones

%funcion para calcular las diferentes expresiones
expresion([X],X).
expresion( L, E1 +  E2 ):- append( L1, L2, L), L1 \= [], L2 \= [],expresion( L1, E1 ),expresion( L2, E2 ).
expresion( L, E1 -  E2 ):- append( L1, L2, L), L1 \= [], L2 \= [],expresion( L1, E1 ),expresion( L2, E2 ).
expresion( L, E1 * E2 ):- append( L1, L2, L), L1 \= [], L2 \= [],expresion( L1, E1 ),expresion( L2, E2 ).
expresion( L, E1 //  E2 ):- append( L1, L2, L), L1 \= [], L2 \= [],expresion( L1, E1 ),expresion( L2, E2 ), K is E2, K\=0.


%2.
%producto de una lista:
prod([X],X):- !.
prod([X|L],P):- prod(L,P1), P is X * P1.

%3.
%producto escalar:
pescalar(L1,L2,_):- long(L1) \= long(L2), fail.
pescalar([X],[Y],X*Y):-!.
pescalar([X1|L1],[X2|L2],Res):- pescalar(L1,L2,Res2), Res is X1 * X2 + Res2.

%4.
%union:
union([],L,L).
union([X|L1],L2,U):- member(X,L2), union(L1,L2,U),!.
union([X|L1],L2,[X|U]):- not(member(X,L2)), union(L1,L2,U).

%interseccion:
interseccion([],_,[]).
interseccion([X1|L1],L2,I):- not(member(X1,L2)), interseccion(L1,L2,I). %si X no pertenece a l2, no se incluye
interseccion([X1|L1],L2,[X1|I]):- member(X1,L2), interseccion(L1,L2,I),!.

%5.
%ultimo de una lista
ultimo([X],X):-!.
ultimo([_|Lista],Ult):- ultimo(Lista,Ult2), concatenar(_,Ult2,Ult),!.

%invertir una lista:
invertir([],[]).
invertir(L,[X|L1]):- concatenar(L2,[X],L), invertir(L2,L1),!.

%6.
%fibonacci (N-2,N-1,Res)
fib(1,1).
fib(2,1).
fib(N,Res):- N>2,N2 is N-2, N1 is N-1, fib(N2,Res2), fib(N1, Res1), Res is Res2+Res1,!. 

%7.
%combinaciones de las caras de los dados (1-6) tales que con N tiradas se obtiene P (L contiene los valores de las tiradas)
dados(0,0,[]).
%N positivo (quedan tiradas), X es un valor de dado y se puede obtener la suma restante (P-X) con N-1 tiradas
dados(P,N, [X|L]):- N>0, pertenece(X, [1,2,3,4,5,6]), P1 is P-X, N1 is N-1, dados(P1,N1,L).

%8.
%lista L, devuelve si N (elemento de L) se puede obtener al sumar el resto de elementos de L
suma_demas([]).
suma_demas(L):- pertConResto(X,L,Resto), suma(Resto,X),!.

%devuelve la suma de L en R
suma([],0).
suma([X|L],R):- suma(L,S1), R is S1+X.

%9.
%lista L, devuelve si N (elemento de L) se puede obtener con los elementos previos de L (hasta N, no incluido)
suma_ants([],0).
suma_ants(L):- concatenar(L1, [X|_], L), suma(L1,X), !.

%10.
%lista con el numero de apariciones de cada elemento
car([],[]).
car([X|L],[[X,Cont]|S]):- car(L,C), pertConResto([X,N],C,S), !,Cont is N+1.
car([X|L], [[X,1]|C]):-car(L,C).

card(L):- car(L,C), write(C).

%11.
%indica si la lista esta ordenada de menor a mayor
esta_ordenada([]).
esta_ordenada([_]):-!.
esta_ordenada([X,Y|L]):- X=<Y, esta_ordenada([Y|L]).

%12.
%deja en L2 la lista ordenada de L1
ordenacion(L1,L2):- permutacion(L1,L2), esta_ordenada(L2),!.

%14.
%ordenar por insercion
ordenacionInsercion([],[]).
ordenacionInsercion([X|L1],L2):- ordenacionInsercion(L1,LOrd), insercion(X,LOrd, L2),!.

insercion(X,[],[X]).
insercion(X,[Y|L1],[Y|L2]):- X>Y, insercion(X,L1,L2). %si X >Y, seguimos buscando
insercion(X,[Y|L1],[X,Y|L1]):- X=<Y.

%16.
%ordenar por merge sort: dividir, sort y merge
divide([],[],[]).
divide([Ini],[Ini],[]).
divide([X1,X2|L1],[X1|La],[X2|Lb]):- divide(L1,La,Lb).

mergeSort([],[]):-!.
mergeSort([X],[X]):-!.
mergeSort(L1,L2):- divide(L1,L1a,L1b), mergeSort(L1a,L2a), mergeSort(L1b, L2b), merge(L2a,L2b,L2),!.

merge(L,[],L).
merge([],L,L).
merge([X|L1],[Y|L2],[X|Res]):- X=<Y, !, merge(L1,[Y|L2],Res).
merge([X|L1],[Y|L2],[Y|Res]):- merge([X|L1],L2,Res).

%17.
%formar palabras de N silabas a partir de las silabas dadas
diccionario(A,N):- mezcla(A,N,Res), escribe(Res),fail.

mezcla(_,0,[]):-!.
mezcla(A,N,[W|S]):- pertenece(W,A), N1 is N-1, mezcla(A,N1,S).

escribe([]):-nl, !.
escribe([X|L]):- write(X), escribe(L).

%18.
%genera los palindromos dada una lista
palindromos(L):- permutacion(L,P), esPalindromo(P), write(P), nl,fail.

esPalindromo([]):-!.
esPalindromo([_]):-!.
esPalindromo([X|L]):- concatenar(L1,[X],L), esPalindromo(L1).

%19.
%asigna los valores a cada letra para que send+more = money

sendMoreMoney:-
    %Como cada numero es diferente, a L' hay que quitarle el valor de la letra que se acaba de anadir
    Nums = [0,1,2,3,4,5,6,7,8,9],
    pertConResto(M,[0,1],_),
    pertConResto(M,Nums,Lm), %Lm: Nums sin el valor de M
    pertConResto(O,Lm,Lmo), %Lmo: L sin el valor de M y O
    pertConResto(R,Lmo,Lmor),
    pertConResto(Y,Lmor,Lmory),
    pertConResto(D,Lmory,Lmoryd),
    pertConResto(N,Lmoryd,Lmorydn),
    pertConResto(E,Lmorydn,Lmorydne),
    pertConResto(S,Lmorydne,_),
    checkSuma([D,N,E,S],[E,R,O,M],[Y,E,N,O],0,M),  
    %orden "inverso" para que coincidan las cifras (decimas, centesimas..)
    
    write('S = '), write(S), nl,
    write('E = '), write(E), nl,
    write('N = '), write(N), nl,
    write('D = '), write(D), nl,
    write('M = '), write(M), nl,
    write('O = '), write(O), nl,
    write('R = '), write(R), nl,
    write('Y = '), write(Y), nl,
    write('   '), write([S,E,N,D]), nl,
    write(' + '), write([M,O,R,E]), nl,
    write('-------------------'), nl,
    write([M,O,N,E,Y]), nl.

%comprueba que la suma 
checkSuma([],[],[],C,C).    %cuando no queda mas que sumar, el digito vale el carry
checkSuma([X1|L1],[X2|L2],[X3|L3],Carry,Cout):-
    X3 is (X1 + X2 + Carry) mod 10,   %la unidad de la respuesta es igual a la suma de las letras y el carry (normalizado, modulo 10) -> (Y=E+D+0, E=R+D+Carry..)
    NewCarry  is (X1 + X2 + Carry) //  10, %el nuevo carry es la parte "sobrante" de calcular la cifra
    checkSuma(L1,L2,L3,NewCarry,Cout).


%20.
%simplifica (linea 70)




%21.
%3 misioneros y 3 canibales tienen que cruzar el rio. En la canoa solo caben 1 o 2 personas. Si los misioneros son minoria, los canibales se los comen.Todos tienen que cruzar la orilla.

%situacion inicial: todos en el lado 1, nadie en el lado 2, estado inicial
val(V):- between(0,3,V).    %

%lado,misioneros,canivales
cruzarRio:- encontrarCamino([lado1,3,3],[lado2,0,0],[[lado1,3,3]]).

%encontrar el camino
encontrarCamino(E1,E1,Pasos):- invertir(Pasos,Sol), write(Sol),nl.

encontrarCamino(E1,E2,Pasos):- pasoLadoLado(E1,En), noIntentado(En,Pasos),encontrarCamino(En,E2,[En|Pasos]),!.

%no intentado
noIntentado(E,Sol):- pertenece(E,Sol), !,fail.
noIntentado(_,_).

%pasar del lado 1 al lado 2 (M1 misioneros y C1 canivales) habiendo M2 misioneros y C2 canivales en el lado 2
pasoLadoLado([lado1,M1,C1],[lado2,M2,C2]):- canoaOcupada(Mis,Can), M2 is M1-Mis, C2 is C1-Can, aSalvo(M2,C2).

%pasar del lado 2 al lado 1
pasoLadoLado([lado2,M2,C2],[lado1,M1,C1]):- canoaOcupada(Mis,Can), M1 is M2+Mis, C1 is C2+Can, aSalvo(M1,C1).

%posible ocupacion de la canoa
canoaOcupada(M,C):- S = [[0,1],[0,2],[1,0],[1,1],[2,0]], member([M,C],S).

%a salvo: cantidad de misiones y canivales valida, los misioneros superan a los canivales y la diferencia (otra orilla) tambien esta a salvo
aSalvo(Mis,Can):- val(Mis), val(Can), noPeligro(Mis,Can), MisLeft is 3-Mis, CanLeft is 3-Can, noPeligro(MisLeft,CanLeft). 

%no hay peligro para los misioneros
noPeligro(0,_).
noPeligro(M,C):- M>=C.





%22.
%eq-split: dividir una lista tal que los elementos a la izquierda sumen igual que los de la derecha
eqSplit(L,S1,S2):- split(L,S1,S2), suma(S1,X), suma(S2,X).
%suma igual que en el ejercicio 8
%suma([],0).
%suma([X|L],N):- suma(L,N1), N is N1+X.

split([],[],[]).
split([X|L],[X|S1],S2):- split(L,S1,S2).
split([X|L],S1,[X|S2]):- split(L,S1,S2).


