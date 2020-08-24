%% LIST REVERSE %%

% push back
pb(X, [], cons(X, nil)).
pb(X, [Y|YS], [Y|ZS]) :- pb(X, YS, ZS).

% list reverse
rev([], []).
rev([X|XS], ZS) :- rev(XS, YS), pb(X, YS, ZS).

