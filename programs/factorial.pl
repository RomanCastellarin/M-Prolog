%% FACTORIAL %%

% base case
fact(0, 1).

% recursive case
fact(X, Y) :- V is X-1, X > 0, fact(V, W), Y is W*X.

