%% GRAPH SEARCH %%

% simple paths R from X to Y
path(X, Y, R) :- search(Y, X, [Y], R).

% depth first search
search(X, X, T, T).
search(X, Y, T, R) :- edge(X, Z), ~member(Z, T), search(Z, Y, [Z|T], R).
search(X, Y, T, R) :- edge(Z, X), ~member(Z, T), search(Z, Y, [Z|T], R).

% is a member of
member(X, [X|_]).
member(X, [_|Y]) :- member(X, Y).

% graph
edge(g,h).    edge(g,d).     edge(e,d).      edge(h,f).     edge(e,f).
edge(a,e).    edge(a,b).     edge(b,f).      edge(b,c).     edge(f,c).

