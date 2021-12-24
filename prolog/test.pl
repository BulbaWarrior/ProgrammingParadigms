word(astante, a,s,t,a,n,t,e).
word(astoria, a,s,t,o,r,i,a).
word(baratto, b,a,r,a,t,t,o).
word(cobalto, c,o,b,a,l,t,o).
word(pistola, p,i,s,t,o,l,a).
word(statale, s,t,a,t,a,l,e).

setOfWords([]).
setOfWords([H|T]):-
    setOfWords(T),
    word(H, _, _, _, _, _, _, _),
    listLacks(T, H).

% ?- length(S, 6), setOfWords(S).
%@ S = [statale, pistola, cobalto, baratto, astoria, astante]


crossword(V1, V2, V3, H1, H2, H3) :-
    word(V1, _, V11, _, V12, _, V13, _),
    word(V2, _, V21, _, V22, _, V23, _),
    word(V3, _, V31, _, V32, _, V33, _),
    word(H1, _, V11, _, V21, _, V31, _),
    word(H2, _, V12, _, V22, _, V32, _),
    word(H3, _, V13, _, V23, _, V33, _),
    setOfWords([V1, V2, V3, H1, H2, H3]).
% ?- crossword(A, B, C, D, E, F).



directTrain(saarbruecken,dudweiler).
directTrain(forbach,saarbruecken).
directTrain(freyming,forbach).
directTrain(stAvold,freyming).
directTrain(fahlquemont,stAvold).
directTrain(metz,fahlquemont).
directTrain(nancy,metz).

travelFromTo(X, Y):-
    directTrain(X, Y).

travelFromTo(X, Z):-
    directTrain(X, Y),
    travelFromTo(Y, Z).


%% ?- travelFromTo(forbach, X).
%@ X = saarbruecken X = dudweiler false.
%?- prolog_flag(color_term, A).




byCar(auckland,hamilton).
byCar(hamilton,raglan).
byCar(valmont,saarbruecken).
byCar(valmont,metz).

byTrain(metz,frankfurt).
byTrain(saarbruecken,frankfurt).
byTrain(metz,paris).
byTrain(saarbruecken,paris).

byPlane(frankfurt,bangkok).
byPlane(frankfurt,singapore).
byPlane(paris,losAngeles).
byPlane(bangkok,auckland).
byPlane(singapore,auckland).
byPlane(losAngeles,auckland).

travel_(X, Y) :-
    byCar(X, Y);
    byTrain(X, Y);
    byPlane(X, Y).

travel(X, Y) :-
    byCar(X, Y);
    byTrain(X, Y);
    byPlane(X, Y).

travel(X, Y):-
    travel_(X, Z),
    travel(Z, Y).

%% ?- travel(valmont, X).

travel(X, Y, goByCar(X, Y)):-
    byCar(X, Y).

travel(X, Y, goByTrain(X, Y)):-
    byTrain(X, Y).

travel(X, Y, goByPlane(X, Y)):-
    byPlane(X, Y).

travel(X, Z, goByCar(X, Y, Path)):-
    byCar(X, Y),
    travel(Y, Z, Path).

travel(X, Z, goByTrain(X, Y, Path)):-
    byTrain(X, Y),
    travel(Y, Z, Path).

travel(X, Z, goByPlane(X, Y, Path)):-
    byPlane(X, Y),
    travel(Y, Z, Path).

%% ?- travel(valmont, raglan, Path).
%@ Path = goByCar(valmont, saarbruecken, goByTrain(saarbruecken, frankfurt, goByPlane(frankfurt, bangkok, goByPlane(bangkok, auckland, goByCar(auckland, hamilton, goByCar(hamilton, raglan)))))) Unknown action:  (h for help)
%@ Action? Path = goByCar(valmont, saarbruecken, goByTrain(saarbruecken, frankfurt, goByPlane(frankfurt, singapore, goByPlane(singapore, auckland, goByCar(auckland, hamilton, goByCar(hamilton, raglan)))))) Path = goByCar(valmont, saarbruecken, goByTrain(saarbruecken, paris, goByPlane(paris, losAngeles, goByPlane(losAngeles, auckland, goByCar(auckland, hamilton, goByCar(hamilton, raglan)))))) Path = goByCar(valmont, metz, goByTrain(metz, frankfurt, goByPlane(frankfurt, bangkok, goByPlane(bangkok, auckland, goByCar(auckland, hamilton, goByCar(hamilton, raglan)))))) Path = goByCar(valmont, metz, goByTrain(metz, frankfurt, goByPlane(frankfurt, singapore, goByPlane(singapore, auckland, goByCar(auckland, hamilton, goByCar(hamilton, raglan)))))) Path = goByCar(valmont, metz, goByTrain(metz, paris, goByPlane(paris, losAngeles, goByPlane(losAngeles, auckland, goByCar(auckland, hamilton, goByCar(hamilton, raglan)))))) [1;31mfalse.[0mn

%% ?- travel(valmont, losAngeles, Path).
%@ Path = goByCar(valmont, saarbruecken, goByTrain(saarbruecken, paris, goByPlane(paris, losAngeles))) Path = goByCar(valmont, metz, goByTrain(metz, paris, goByPlane(paris, losAngeles))) [1;31mfalse.[0m


connected(1,2).
connected(3,4).
connected(5,6).
connected(7,8).
connected(9,10).
connected(12,13).
connected(13,14).
connected(15,16).
connected(17,18).
connected(19,20).
connected(4,1).
connected(6,3).
connected(4,7).
connected(6,11).
connected(14,9).
connected(11,15).
connected(16,12).
connected(14,17).
connected(16,19).

path_(X, Y):-
    connected(X, Y);
    connected(Y, X).
listLacks([], _).
listLacks([Head|Tail], E):-
    Head \= E,
    listLacks(Tail, E).
% ?- listLacks([1, 2, 3], 3).
path(X, Y, Visited):-
    path_(X, Y),
    listLacks(Visited, Y).
path(X, Z, Visited):-
    path_(X, Y),
    listLacks(Visited, Y),
    path(Y, Z, [Y|Visited]).

%% ?- path(1, 16, []).

node(X):-
    connected(X, _);
    connected(_, X).
%% ?- node(X).
%@ X = 1.
setOfNodes([]).
setOfNodes([H|T]):-
    setOfNodes(T),
    node(H),
    listLacks(T, H).

%?- setOfNodes(X).
%@ X = [] X = [1] X = [3] X = [5]
mypath(X, Y, _):-
    path_(X, Y).
mypath(X, Z, Visited):-
    path_(X, Y),
    listLacks(Visited, Y),
    mypath(Y, Z, [Y|Visited]).


path(X, Y, Visited, Visited):-
    path_(X, Y).
path(X, Z, Visited, Path):-
    path_(X, Y),
    listLacks(Visited, Y),
    path(Y, Z, [Y|Visited], Path).
%?- path(1, 7, [], Path).
%@ Path = [4, 1, 2] Path = [8, 7, 4, 1, 2] Path = [4] Path = [8, 7, 4] [1;31mfalse.[0m




%?- setOfNodes(X), listLacks(X, 1), length(X, 4).
%?- mypath(1, 16, []).
%@ [1mtrue [0m[1mtrue [0m[1mtrue [0m[1mtrue [0m[1mtrue [0m[1mtrue [0m[1;31mfalse.[0m
%






%?- listOfNodes(Visited), listSet(Visited, Visited).
