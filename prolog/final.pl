p([], []).
p([[]|T], T1):-
    !, p(T, T1).
p([H|T], Res):-
    \+ \+ H = [_|_],!,
    p(H, H1),
    p(T, T1),
    concat(H1, T1, Res).
p([H|T], Res):-
    H \= [_|_],
    !,
    p(T, T1),
    Res = [H|T1].


concat([], L, L).
concat([H|T], Lst, Res):-
    concat(T, [H|Lst], Res).
