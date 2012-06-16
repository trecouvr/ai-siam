
% index(+X,+Y,?N).
% index(?X,?Y,+N).
index(X,Y,N) :- var(X), x_y_case(X,Y,N), !.
index(X,Y,N) :- var(Y), x_y_case(X,Y,N), !.
index(X,Y,N) :- num_case(X,Y,N).
num_case(X,Y,N) :- N is X+Y*10.
x_y_case(X,Y,N) :- X is N mod 10, Y is N//10.

% next_case(+C, +Dir, ?Next).
% next_case(+C, ?Dir, +Next).
next_case(NumCase, e, NextCase) :- index(X,Y,NumCase), X2 is X+1, index(X2,Y,NextCase), !.
next_case(NumCase, w, NextCase) :- index(X,Y,NumCase), X2 is X-1, index(X2,Y,NextCase), !.
next_case(NumCase, s, NextCase) :- index(X,Y,NumCase), Y2 is Y-1, index(X,Y2,NextCase), !.
next_case(NumCase, n, NextCase) :- index(X,Y,NumCase), Y2 is Y+1, index(X,Y2,NextCase), !.

prev_case(Current, Dir, Prev) :- direction_opposee(Dir,Opp), next_case(Current, Opp, Prev).

% clean_position(+(X,Y), ?CleanedPos).
% permet de s'assurer que la case est valide (comprise dans un carré de 5x5 ou =0 pour l'exterieur)
clean_position(P,0) :- index(X,_,P), X =< 0, !.
clean_position(P,0) :- index(X,_,P), X > 5, !.
clean_position(P,0) :- index(_,Y,P), Y =< 0, !.
clean_position(P,0) :- index(_,Y,P), Y > 5, !.
clean_position(P,P).

% clean une liste de positions plateau
clean_positions([],[]) :- !.
clean_positions([(P,O)|Q],[(P2,O)|Q2]) :- clean_position(P,P2), clean_positions(Q,Q2).

% clean les positions d'un plateau
clean_positions_plateau([E,R,M,J], [E2,R2,M,J]) :-
	clean_positions(E,E2),
	clean_positions(R,R2).

direction_opposee(e,w).
direction_opposee(s,n).
direction_opposee(w,e).
direction_opposee(n,s).

% case vide
case_vide(_, 0) :- !.
case_vide([E,R,M,_], Position) :- \+member((Position,_), E), \+member((Position,_), R), \+member(Position, M).
case_vide(P,X,Y) :- index(X,Y,N), case_vide(N, P).

% est-ce que un animal est dehors ?
check_dehors([E,_,_,e]) :- member((0,_),E), !.
check_dehors([_,R,_,r]) :- member((0,_),R), !.

% est-ce que l'animal sur la positione est à moi
is_mine([E,_,_,e], NumCase, (NumCase, O)) :- member((NumCase,O), E).
is_mine([_,R,_,r], NumCase, (NumCase, O)) :- member((NumCase,O), R).

case_bordure(NumCase) :- member(NumCase, [11,12,13,14,15,25,35,45,55,54,53,52,51,41,31,21]).

% la case est dans le plateau ??
case_valid(NumCase) :- member(NumCase, [11,12,13,14,15,21,22,23,24,25,31,32,33,34,35,41,42,43,44,45,51,52,53,54,55]).

% l'orientation est valide
orientation_valid(O) :- member(O, [n,s,e,w]).

% joueur adverse
enemy(e,r).
enemy(r,e).

infini(10000).

max(A,B,B) :- B > A, !.
max(A,_,A).

liste_joueur([E,_,_,e], E).
liste_joueur([_,R,_,r], R).


switch_joueur(e,r).
switch_joueur(r,e).

joueur([_E,_R,_M,J], J).
opposant([_E,_R,_M,J], J2) :- switch_joueur(J,J2).


% récupérer les cases autour d'une case
%cases_autour(P, [C11,C12,C13,C21,C23,C31,C32,C33]) :-
cases_autour(P, [C12,C21,C23,C32]) :-
	index(X,Y,P),
	Xp is X+1, Xm is X-1, Yp is Y+1, Ym is Y-1,
	%index(Xm,Ym,C11), 	index(X,Ym,C12), 	index(Xp,Ym,C13),
	index(X,Ym,C12),
	index(Xm,Y,C21), 						index(Xp,Y,C23),
	%index(Xm,Yp,C31), 	index(X,Yp,C32), 	index(Xp,Yp,C33).
	index(X,Yp,C32).

cases_diago(C, [C11,C13,C33,C31]) :-
	index(X,Y,C),
	Xp is X+1, Xm is X-1, Yp is Y+1, Ym is Y-1,
	index(Xm,Ym,C11), index(Xp,Ym,C13),
	index(Xm,Yp,C31), index(Xp,Yp,C33).
