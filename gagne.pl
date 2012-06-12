
montagne_dehors([P|Q], P2) :-
	index(X,Y,P),
	0 < X, X =< 5,
	0 < Y, Y =< 5,
	montagne_dehors(Q,P2),
	!.
montagne_dehors([P|_], P) :- !.
	
dir_pousse(M, w) :- index(X,_,M), X =< 0, !.
dir_pousse(M, e) :- index(X,_,M), X > 5, !.
dir_pousse(M, s) :- index(_,Y,M), Y =< 0, !.
dir_pousse(M, n) :- index(_,Y,M), Y > 5, !.

% gagnat(+Plateau, ?Gagnant).
gagnant([E,R,M,_], G) :-
	montagne_dehors(M,Md),
	dir_pousse(Md, Dir),
	gagnant(E,R,M, (Md,Dir), G),
	!.

% éléphant bon sens de poussée
gagnant(E, _, _, (P,Dir), e) :-
	direction_opposee(Dir, Dopp),
	next_case(P, Dopp, X),
	member((X,Dir),E),
	!.
% rino bon sens de poussée
gagnant(_, R, _, (P,Dir), r) :-
	direction_opposee(Dir, Dopp),
	next_case(P, Dopp, X),
	member((X,Dir),R),
	!.
% éléphant mauvais sens de poussée
gagnant(E, R, M, (P,Dir), G) :-
	direction_opposee(Dir, Dopp),
	next_case(P, Dopp, X),
	member((X,_),E),
	gagnant(E,R,M, (X,Dir), G),
	!.
% rino mauvais sens de poussée
gagnant(E, R, M, (P,Dir), G) :-
	direction_opposee(Dir, Dopp),
	next_case(P, Dopp, X),
	member((X,_),R),
	gagnant(E,R,M, (X,Dir), G),
	!.
% montagne
gagnant(E, R, M, (P,Dir), G) :-
	direction_opposee(Dir, Dopp),
	next_case(P, Dopp, X),
	member(X,M),
	gagnant(E,R,M, (X,Dir), G),
	!.
