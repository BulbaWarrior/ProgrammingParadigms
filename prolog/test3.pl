minimum([], M, M).
minimum([H|T], Acc, Min):-
    H < Acc,
    minimum(T, H, Min).


minimum([H|T], Acc, Min):-
    H >= Acc,
    minimum(T, Acc, Min).

minimum([H|T], X):-
    minimum(T, H, X).
%?- minimum([3, 6, 2, 5, 4, 7], X).
%@ X = 2 false.

remove(_, [], []).
remove(X, [X|T], T1):-
    !,
    remove(X, T, T1).

remove(X, [H|T], [H|T1]):-
    remove(X, T, T1).

%?- remove(e, [a, p, p, l,e ,p,i,e], X).

removeU(_, [], []).
removeU(Term, [H|T], T1):-
    \+ Term = T, !,
    removeU(Term, T, T1).

removeU(Term, [H|T], [H|T1]):-
    removeU(Term, T, T1).

nat(0).
nat(N):-
    nat(K),
    N is K + 1.

nat(0, 0):- !.
nat(0, Max):- Max > 0.
nat(N, Max):-
    M is Max - 1,
    nat(K, M),
    N is K + 1.

prime_(N):-
    nat(X, N),
    nat(Y, N),
    N is X * Y.

prime(N) :-
    nat(N),
    \+ prime_(N).
