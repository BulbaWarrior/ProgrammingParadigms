
sublist([], _).
sublist([H|T], [H|T1]):-
    sublist(T, T1).
sublist(L1, [_|T]):-
    sublist(L1, T).

%?- sublist([2, 3, 5, 6], [1, 2, 3, 4, 5]).
%@ true false.

prefix([], _).
prefix([H|T], [H|T1]):- prefix(T, T1).

search([], _, _).
search(L1, L2, 0):-
    prefix(L1, L2).

search(L1, [_|T], N):-
    search(L1, T, N1),
    N is N1 + 1,
    N > 0.

%?- search([a, b, a], [c, a, b, a, b, a, d], Pos).
%@ Pos = 1 Pos = 3 false.
