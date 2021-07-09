:- use_module(library(clpfd)).

ejemplo(0,   26, [1,2,5,10] ).  % Solution: [1,0,1,2]
ejemplo(1,  361, [1,2,5,13,17,35,157]).


%como pagar una cierta cantidad dadas varias monedas
main:- 
    ejemplo(1,Amount,Coins),
    nl, write('Paying amount '), write(Amount), write(' using the minimal number of coins of values '), write(Coins), nl,nl,
    length(Coins,N), 
    length(Vars,N),
    %Vars es una lista de N elementos (uno por cada tipo de moneda)
    %cada elemento se usa o no: dominio de 0 a Amount
    Vars ins 0..Amount,
    %restricciones: Vars tiene que ser una expresion numerica de Coins (suma y multiplicacion)
    expr(Vars,Coins, Expr),
    Expr #= Amount,
    %la expresion debe ser igual que la cantidad
    %minimo numero de monedas -> cogemos la suma de los elementos de Vars
    exprSuma(Vars, ExprSuma),

    %labeling: minima suma de Vars
    labeling( [min(ExprSuma)], Vars),

    %impresion
    NumCoins is ExprSuma,
    nl, write(NumCoins), nl,write(Vars), nl,nl, halt.

expr([],[],0).
expr([X|Vars],[K|Coins], Suma+K*X):- expr(Vars,Coins, Suma).

exprSuma([X],X):-!.
exprSuma([X|Vars], X+Cont):- exprSuma(Vars,Cont).



%dominio
%restricciones
%labeling
%impresion
