fact(0, 1).
fact(X, Y) :- V is X-1, X > 0, fact(V, W), Y is W*X.

% push back
pb(X, [], cons(X, nil)).
pb(X, [Y|YS], [Y|ZS]) :- pb(X, YS, ZS).

% list reverse
rev([], []).
rev([X|XS], ZS) :- rev(XS, YS), pb(X, YS, ZS).

% test comparison
twice(X, Y) :- Y is 2*X, Y = 300/10. 

% test negation as failure

friends(rom, seba).
friends(seba, lucas).
friends(seba, ana).
friends(ana, ori).
enemies(X, Y) :- ~friends(X, Y).
friends(X, Y) :- friends(X, Z), friends(Z, Y). 

%% more test
child(john,sue).     child(john,sam).   
child(jane,sue).     child(jane,sam).   
child(sue,george).   child(sue,gina). 

male(john).   male(sam).     male(george). 
female(sue).  female(jane).  female(june). 

parent(Y,X) :- child(X,Y).
father(Y,X) :- child(X,Y), male(Y).
opp_sex(X,Y) :- male(X), female(Y). 
opp_sex(Y,X) :- male(X), female(Y). 
grand_father(X,Z) :- father(X,Y), parent(Y,Z).

% GRAPHS

search(X, X, T, T).
search(X, Y, T, R) :- edge(X, Z), ~member(Z, T), search(Z, Y, [Z|T], R).
search(X, Y, T, R) :- edge(Z, X), ~member(Z, T), search(Z, Y, [Z|T], R).

member(X, [X|_]).
member(X, [_|Y]) :- member(X, Y).

edge(g,h).    edge(g,d).     edge(e,d).      edge(h,f).     edge(e,f).
edge(a,e).    edge(a,b).     edge(b,f).      edge(b,c).     edge(f,c).

% CUT OPERATOR


