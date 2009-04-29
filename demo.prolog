% comment

child(sazae, namihei).
child(sazae, fune).
child(katsuo, namihei).
child(katsuo, fune).
child(wakame, namihei).
child(wakame, fune).
child(tara, sazae).
child(tara, masuo).

?- child(X, fune).
;
;
;

grandChild(X, Z) :- child(X, Y), child(Y, Z).
?- grandChild(X, Y).
;
;

t(1).
t(2).
t(3).
perm(X, Y, Z) :- t(X), t(Y), t(Z).
?- perm(X, Y, X).
;
;
;
;
;
;
;
;
;

append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).

?- append([a,b,c], [d,e], X).

reverse([],[]).
reverse([X|Xs], Zs) :- reverse(Xs, Ys), append(Ys, [X], Zs).

?-reverse([a,b,c,d,e], X).

acReverse(Xs, Ys) :- acReverse(Xs, [], Ys).
acReverse([X|Xs], Acc, Ys) :- acReverse(Xs, [X|Acc], Ys).
acReverse([], Ys, Ys).

?-acReverse([a,b,c,d,e], X).

