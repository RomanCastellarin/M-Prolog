X < 45 + (X + 3 * ( 777 - 5) /2) * -44 + Ya
45 + -(2 + 3 * ( 777 - 5) /2) * -44 +9
[ 1 | [ 2 | [ 3 , 4 | [] ]]]
% TESTING FILE FOR THE  M-PROLOG PROGRAMMING LANGUAGE   
    
    % rev(cons(1,cons(2,cons(3,cons(4,cons(5,nil))))), What).

% rev(cons(1,cons(2,nil)), What
     % pb(5, nil, What)

pb(X, nil, cons(X, nil)).
pb(X, cons(Y,YS), cons(Y, ZS)) :- pb(X, YS, ZS).

rev(nil, nil).
rev(cons(X,XS), ZS) :- rev(XS, YS), pb(X, YS, ZS).

fact(0, 1).
fact(X, Y) :- X > -1, V is X-1, fact(V, W), Y is V*(A-45*( W -8)) / (45 + W).   

%negar(X) :- rev(X,X), !, pb(X,X,X).

% revappend([], Ys, Ys).
% revappend([X|Xs], Ys, Zs) :- revappend(Xs, [X|Ys], Zs).
% reverse(Xs,Ys) :- revappend(Xs,[],Ys).

