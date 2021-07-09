symbolicOutput(0).  % set to 1 to see symbolic output only; 0 otherwise.

%% A bus company operates services between a set of different cities.
%% We want to design the routes of the buses for a given week.
%% Every day a bus must travel from one city to a DIFFERENT one, and
%% stop there waiting until the next day. The following additional
%% constraints need to be considered:
%%
%% - Every day, there is exactly one bus departing from every city (we
%%   can assume numCities = numBuses)
%%
%% - For every pair of different cities C1 and C2, there is at least one bus that goes from
%%   C1 to C2 (on a certain day of the week). 
%%
%% - No bus can travel more than a total of maxDist kms on two consecutive days.

%%%%%% Example input:
numBuses(5).
numDays(7).
cities([mad,bcn,val,bil,zar]).
maxDist(1000).

% madrid->x
dist(mad,mad,0).
dist(mad,bcn,630).
dist(mad,val,360).
dist(mad,bil,400).
dist(mad,zar,320).
%barcelona->x
dist(bcn,mad,630).
dist(bcn,bcn,0).
dist(bcn,val,350).
dist(bcn,bil,610).
dist(bcn,zar,320).
%valencia->x
dist(val,mad,360).
dist(val,bcn,350).
dist(val,val,0).
dist(val,bil,610).
dist(val,zar,310).
%bilbao->x
dist(bil,mad,400).
dist(bil,bcn,610).
dist(bil,val,610).
dist(bil,bil,0).
dist(bil,zar,300).
%zaragoza->x
dist(zar,mad,320).
dist(zar,bcn,320).
dist(zar,val,310).
dist(zar,bil,300).
dist(zar,zar,0).


%%%%%% Some helpful definitions to make the code cleaner:

day(D):-                    numDays(N),between(1,N,D).
notLastDay(D):-             numDays(N), N1 is N-1, between(1,N1,D).
consecutiveDays(D1,D2):-    numDays(N), N1 is N-1, between(1,N1,D1), D2 is D1+1.
consecutiveDays(D1,D2,D3):- numDays(N), N1 is N-2, between(1,N1,D1), D2 is D1+1, D3 is D2+1.
city(C):-                   cities(L),member(C,L).
bus(B):-                    numBuses(N), between(1,N,B).
trip(C1-C2):-               cities(L), member(C1,L), member(C2,L), C1 \= C2.


%%%%%%  1. SAT Variables:

%el bus Bus va de C1 a C2 el dia Dia
satVariable( camino(Bus,C1,C2,Dia)):- bus(Bus), trip(C1-C2), day(Dia).
%el bus Bus esta en City el dia Dia
satVariable( bdc(Bus,Dia,City) ) :- bus(Bus), day(Dia), city(City).


%%%%%%  2. Clause generation:

writeClauses:- nl,
    unDesplazamientoAlDia, %cada bus sale a otra ciudad cada dia
    unaCiudadDestino,   %cada ciudad recibe un bus cada dia
    unTripMinimo,       %se tiene que ir de C1->C2 y de C2->C1
    checkDistancia,     %comprobamos que la distancia maxima no se supera
    caminoImplicaBDC,   %un camino implica dos bdc (D y D+1)
    bdcImplicaCamino,   %implicacion de clausulas (una bdc implica un camino
    true,!.
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.


%por cada dia, cada bus se tiene que desplazar de la ciudad en la que esta a otra
unDesplazamientoAlDia:- bus(B), day(D), findall(
    camino(B,C1,C2,D),trip(C1-C2),Trips), exactly(1,Trips),fail.
unDesplazamientoAlDia.

%por cada ciudad, cada dia tiene que recibir un bus 
unaCiudadDestino:- city(C), day(D), findall(bdc(B,D,C),bus(B),
    Trips), exactly(1,Trips),fail.
unaCiudadDestino.


%cada semana se tiene que hacer el recorrido (por lo menos una vez) de C1 a C2 y de C2 a C1: para cada trip, buscamos si se ha hecho el camino (para cualquier bus B y cualquier dia D que no sea el ultimo)
unTripMinimo:- trip(C1-C2), findall(camino(B,C1,C2,D),
    (bus(B), notLastDay(D)), Trips), atLeast(1,Trips),fail.
unTripMinimo.

%posibles recorridos maximos (trip(C1-C2),trip(C2-C3) tal que distancia es mayor que distancia maxima (NO puede pasar -> clausulas negadas)
checkDistancia:- bus(B), consecutiveDays(D1,D2), trip(C1-C2),trip(C2-C3), dist(C1,C2,Dist1), dist(C2,C3,Dist2), DistBus is Dist1+Dist2, maxDist(Max),DistBus>Max, writeClause([-camino(B,C1,C2,D1), -camino(B,C2,C3,D2)]),fail.
checkDistancia.

%clausulas para la implicacion bdc -> camino: si hay un camino (B,C1,C2,D1) debe haber bdc(B,D1,C1) y bdc(B,D2,C2) --> hay camino O no hay ninguna de las dos bdc
bdcImplicaCamino:- bus(B), trip(C1-C2), consecutiveDays(D1,D2),
    writeClause([camino(B,C1,C2,D1), -bdc(B,D1,C1),
    -bdc(B,D2,C2)]), fail.
bdcImplicaCamino.

%clausulas para la implicacion camino -> si hay un bdc(B,D1,C1) y un bdc(B,D2,C2) debe haber un camino(B,C1,C2,D1) --> hay camino O no hay bdc1 Y hay camino O no hay bdc2
caminoImplicaBDC:- bus(B), trip(C1-C2), consecutiveDays(D1,D2),
    writeClause([-camino(B,C1,C2,D1),bdc(B,D1,C1)]), writeClause([-camino(B,C1,C2,D1),bdc(B,D2,C2)]),fail.
caminoImplicaBDC.



%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:

displaySol(M):- nl, day(D), write('Day '), write(D), write(': '),
    findall(C-bus(B),member(bdc(B,D,C), M), L), write(L), nl, fail.
displaySol(M):- nl, trip(C1-C2), nl, write('Trip '), write(C1-C2), write(':'),
    findall(B-D, (member(bdc(B,D,C1),M), member(bdc(B,D2,C2),M), D2 is D+1), L), 
    member(B1-D1,L), write(' (Bus '), write(B1), write(', Day '), write(D1), write(') '), fail.
displaySol(M):- nl, bus(B), nl, write('Consec. distances for bus '), write(B), write(': '),
    consecutiveDays(D1,D2,D3), member(bdc(B,D1,C1),M), member(bdc(B,D2,C2),M), member(bdc(B,D3,C3),M),
    dist(C1,C2,Dist1), dist(C2,C3,Dist2), Dist is Dist1 + Dist2, write(Dist), write(' '), fail.    
displaySol(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything below is given as a standard library, reusable for solving 
%    with SAT many different problems.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits ):- symbolicOutput(1), write( Var ), write(' <--> or('), write(Lits), write(')'), nl, !. 
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.

%% expressOr(a,[x,y]) genera 3 clausulas (como en la TransformaciÃ³n de Tseitin):
%% a == x v y
%% x -> a       -x v a
%% y -> a       -y v a
%% a -> x v y   -a v x v y

% Express that Var is equivalent to the conjunction of Lits:
expressAnd( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> and('), write(Lits), write(')'), nl, !. 
expressAnd( Var, Lits):- member(Lit,Lits), negate(Var,NVar), writeClause([ NVar, Lit ]), fail.
expressAnd( Var, Lits):- findall(NLit, (member(Lit,Lits), negate(Lit,NLit)), NLits), writeClause([ Var | NLits]), !.


%%%%%% Cardinality constraints on arbitrary sets of literals Lits:

exactly(K,Lits):- symbolicOutput(1), write( exactly(K,Lits) ), nl, !.
exactly(K,Lits):- atLeast(K,Lits), atMost(K,Lits),!.

atMost(K,Lits):- symbolicOutput(1), write( atMost(K,Lits) ), nl, !.
atMost(K,Lits):-   % l1+...+ln <= k:  in all subsets of size k+1, at least one is false:
      negateAll(Lits,NLits),
      K1 is K+1,    subsetOfSize(K1,NLits,Clause), writeClause(Clause),fail.
atMost(_,_).

atLeast(K,Lits):- symbolicOutput(1), write( atLeast(K,Lits) ), nl, !.
atLeast(K,Lits):-  % l1+...+ln >= k: in all subsets of size n-k+1, at least one is true:
      length(Lits,N),
      K1 is N-K+1,  subsetOfSize(K1, Lits,Clause), writeClause(Clause),fail.
atLeast(_,_).

negateAll( [], [] ).
negateAll( [Lit|Lits], [NLit|NLits] ):- negate(Lit,NLit), negateAll( Lits, NLits ),!.

negate( -Var,  Var):-!.
negate(  Var, -Var):-!.

subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).


%%%%%% main:

main:-  symbolicOutput(1), !, writeClauses, halt.   % print the clauses in symbolic form and halt
main:-  initClauseGeneration,
        tell(clauses), writeClauses, told,          % generate the (numeric) SAT clauses and call the solver
        tell(header),  writeHeader,  told,
        numVars(N), numClauses(C),
        write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
        shell('cat header clauses > infile.cnf',_),
        write('Calling solver....'), nl,
        shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
        treatResult(Result),!.

treatResult(20):- write('Unsatisfiable'), nl, halt.
treatResult(10):- write('Solution found: '), nl, see(model), symbolicModel(M), seen, displaySol(M), nl,nl,halt.
treatResult( _):- write('cnf input error. Wrote anything strange in your cnf?'), nl,nl, halt.
    

initClauseGeneration:-  %initialize all info about variables and clauses:
        retractall(numClauses(   _)),
        retractall(numVars(      _)),
        retractall(varNumber(_,_,_)),
        assert(numClauses( 0 )),
        assert(numVars(    0 )),     !.

writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w(-Var):- symbolicOutput(1), satVariable(Var), write(-Var), write(' '),!. 
w( Var):- symbolicOutput(1), satVariable(Var), write( Var), write(' '),!. 
w(-Var):- satVariable(Var),  var2num(Var,N),   write(-), write(N), write(' '),!.
w( Var):- satVariable(Var),  var2num(Var,N),             write(N), write(' '),!.
w( Lit):- told, write('ERROR: generating clause with undeclared variable in literal '), write(Lit), nl,nl, halt.


% given the symbolic variable V, find its variable number N in the SAT solver:
:-dynamic(varNumber / 3).
var2num(V,N):- hash_term(V,Key), existsOrCreate(V,Key,N),!.
existsOrCreate(V,Key,N):- varNumber(Key,V,N),!.                            % V already existed with num N
existsOrCreate(V,Key,N):- newVarNumber(N), assert(varNumber(Key,V,N)), !.  % otherwise, introduce new N for V

writeHeader:- numVars(N),numClauses(C), write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-     retract( numClauses(N0) ), N is N0+1, assert( numClauses(N) ),!.
newVarNumber(N):- retract( numVars(   N0) ), N is N0+1, assert(    numVars(N) ),!.

% Getting the symbolic model M from the output file:
symbolicModel(M):- get_code(Char), readWord(Char,W), symbolicModel(M1), addIfPositiveInt(W,M1,M),!.
symbolicModel([]).
addIfPositiveInt(W,L,[Var|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, varNumber(_,Var,N),!.
addIfPositiveInt(_,L,L).
readWord( 99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ c
readWord(115,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ s
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================
