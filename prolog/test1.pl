:- use_module(library(clpfd)).
x_plus_six(X, Y):-
    Y #= X + 6.
%?- x_plus_six(X, 1).
%@ X = -5.
%?- x_plus_six(X, Y).
%@ X+6#=Y.

groupmates(X, Y):-
    student(X, N),
    student(Y, N).


%% friend(alisa, Y), friend(Y, Z) ->
%%     friend(alisa, bob), friend(bob, Z) ->
%%         friend(alisa, bob), friend(bob, chloe);
%%         friend(alisa, bob), friend(bob, edward);
%%     friend(alisa, denise), friend(denise, Z) ->
%%         friend(alisa, denise), friend(denise, edward)
%%

unary(z).
unary(s(X)):-
    unary(X).


add(z, X, X):- unary(X).
add(s(X), Y, s(Z)):- add(X, Y, Z).
%%?- add(s(z), s(s(z)), X).
%@ X = s(s(s(z))).


mult(z, _, z).
mult(s(X), Y, Z):-
    mult(X, Y, Z1),
    add(Y, Z1, Z).

%?- X #= Y + Z.

list_reversed([], Acc, Acc).
list_reversed([H|T], Acc, Res):-
    list_reversed(T, [H|Acc], Res).

list_reversed(L, R):-
    list_reversed(L, [], R).
%?- list_reversed([1, 2, 3], X).
%@ X = [3, 2, 1].
