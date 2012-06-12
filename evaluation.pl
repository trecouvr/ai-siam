

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


% animal en dehors de la carte
score_animal((0,_), _, 0) :- !.

% plus de montagnes
score_animal(_, [], 0) :- !.

% animal à côté d'une montagne et dans la bonne direction	
score_animal((P,Dir), [(M, Lp)|Q], S) :- member(P,Lp), next_case(P, Dir, M), score_animal((P,Dir), Q, S2), S is 5 + S2, !.

% animal à côté d'une montagne mais dans la mauvaise direction
score_animal((P,Dir), [(_, Lp)|Q], S) :- member(P,Lp), score_animal((P,Dir), Q, S2), S is 2 + S2, !. 

% sinon
score_animal(P, [_|Q], S) :- score_animal(P, Q, S).

score_animaux([], _, 0) :- !.
score_animaux([A|Q], M, S) :- score_animal(A, M, S2), score_animaux(Q,M,S3), S is S2+S3.

score_elep_rino(E,R,M, Se, Sr) :-
	findall((Montagne,Lp), (member(Montagne, M), cases_autour(Montagne, Lp)), Ms),
	score_animaux(E, Ms, Se),
	score_animaux(R, Ms, Sr).

% score maximal si le joueur courant gagne
score_plateau([E,R,M,J], 10000) :- gagnant([E,R,M,J], J), !.
% score minimal si le joueur courant perd
score_plateau([E,R,M,J], -10000) :- enemy(J,J2), gagnant([E,R,M,J], J2), !.

score_plateau([E,R,M,e], S) :-
	score_elep_rino(E,R,M,Se,Sr),
	S is Se - Sr,
	!.

score_plateau([E,R,M,e], S) :-
	score_elep_rino(E,R,M,Se,Sr),
	S is Sr - Se,
	!.

