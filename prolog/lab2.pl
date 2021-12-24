:- use_module(library(clpfd)).

prefix([], _).
prefix([H|T], [H|T1]):- prefix(T, T1).
%?- prefix(X, [1, 2, 3]).
%@ X = [] X = [1] X = [1, 2] X = [1, 2, 3] false.


interleave([], X, X).
%interleave(X, [], X).
interleave([H1|T1], [H2|T2], [H1, H2| T3]):-
    interleave(T1, T2, T3).
%?-interleave([1, 2], [a, b], X).
%@ X = [1, a, 2, b].
%?-interleave(X, Y, [1, 2, 3, 4]).
%@ X = [],
%@ Y = [1, 2, 3, 4] X = [1],
%@ Y = [2, 3, 4] X = [1, 3],
%@ Y = [2, 4] false.

%?- member(3, [X, 3, Y]).

list_squares([], []).
list_squares([Num|Nums], [Square|Squares]):-
    Square #= Num * Num,
    list_squares(Nums, Squares).
%?- list_squares(X, [1, 4, 9, 16]).
%@ X = [_A, _B, _C, _D],
%@ _A in -1\/1,
%@ _B in -2\/2,
%@ _C in -3\/3,
%@ _D in -4\/4.


select([H|T], H, T).
select([H|T], E, [H|Res]):-
    select(T, E, Res).
%?- select([1, 2], 3, R).
%@ R = [1, 2].
%@ R = [2] false.
%

%% anagram(X, Y):-
%%     length(X, N),
%%     length(Y, N),
%%     anagram_(X, Y).

anagram([], []).
anagram([H|T], L):-
    anagram(T, L1),
    select(L, H, L1).

%?- anagram([d,o,r,m,i,t,o,r,y], [d,i,r,t,y, r,o,o,m]).
%?- anagram([1, 2], X).
%@ X = [1, 2] X = [2, 1] false.
%@ X = [1, 2] X = [2, 1] X = [2, 1] false.




color(red).
color(blue).
color(green).
nation(spanish).
nation(english).
nation(japanese).
pet(snail).
pet(jaguar).
pet(zebra).

fact(Color, Nationality, Pet):-
    color(Color),
    nation(Nationality),
    pet(Pet).

listOfFacts([], _).
listOfFacts([H|T], Used):-
    H = fact(X, Y, Z),
    \+ member(X, Acc),
    \+ member(Y, Acc),
    \+ member(Z, Acc),
    listOfFacts(T, [X, Y, Z| Used]).

solution(X):-
    X = [fact(C1, N1, snail), fact(blue, N2, P2), fact(C3, japanese, P3)],
    listOfFacts(X, []),
    TrueFact1 = fact(red, english, _),
    TrueFact2 = fact(_, spanish, jaguar),
    member(TrueFact1, X),
    member(TrueFact2, X).

%?- solution(X).
sorted([]).
sorted([X]).
sorted([A, B|Tail]):-
    B #>= A,
    sorted([B|Tail]).

insert([H|L], Num, [H, Num|Tail]):-
    Num #>= H.
insert([])

list_current_sorted([],Current, Current).
list_current_sorted([Num|Tail], Current, _):-
    insert(Current, Num, Next)
    list_current_sorted(Tail, Next, _).


%?-sorted([3, 2, 1]).
%@ true false.
