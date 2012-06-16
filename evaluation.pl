


objects_in_pousse(Plateau, P, _Dir, M, M, A, _RA, A) :- case_vide(Plateau, P), !.
objects_in_pousse([E,R,Ms,J], P, Dir, _, M, _RRA, RA, A) :-
	member(P,Ms),
	next_case(P,Dir,Next),
	objects_in_pousse([E,R,Ms,J], Next, Dir, P, M, RA, RA, A), !.
objects_in_pousse([E,R,Ms,J], P, Dir, RM, M, RRA, _RA, A) :-
	append(E,R,L),
	member((P,_), L),
	next_case(P,Dir,Next),
	objects_in_pousse([E,R,Ms,J], Next, Dir, RM, M, RRA, P, A), !.
objects_in_pousse(Plateau, P, Dir, R, M, RRA, RA, A) :-
	next_case(P,Dir,Next),
	objects_in_pousse(Plateau, Next, Dir, R, M, RRA, RA, A), !.


bonne_pousse_possible(Plateau, P, Dir) :-
	next_case(P, Dir, Next),
	\+ case_vide(Plateau, Next),
	pousse_possible(Plateau, P, Dir),
	objects_in_pousse(Plateau, Next, Dir, -1, M, P, P, A),
	M \= -1, is_mine(Plateau,A,_), !.
	
% animal en dehors de la carte
score_animal(_, (0,_), _, 0) :- !.

% plus de montagnes
score_animal(_, _, [], 0) :- !.

% animal pouvant pousser une montagne
%score_animal(Plateau, (P,Dir), [(M, Lp)|_Q], 10) :- member(P,Lp), next_case(P, Dir, M), pousse_possible(Plateau, P, Dir), !.

% animal pouvant pousser
%score_animal(Plateau, (P,Dir), _M, 10) :- pousse_possible(Plateau, P, Dir), !.


% animal pouvant pousser une montagne dont un allier est à côté
score_animal(Plateau, (P,Dir), _M, 10) :-
	bonne_pousse_possible(Plateau, P, Dir),
	!.

% animal à côté d'une montagne au bord dans la bonne direction
score_animal(Plateau, (P,Dir), [(M, Lp)|Q], S) :-
	member(P,Lp),
	next_case(P, Dir, M),
	case_bordure(M),
	next_case(M, Dir, Dehors),
	\+ case_valid(Dehors),
	score_animal(Plateau, (P,Dir), Q, S2),
	max(8,S2,S), !.

% animal à côté d'une montagne et dans la bonne direction	
score_animal(Plateau, (P,Dir), [(M, Lp)|Q], S) :- member(P,Lp), next_case(P, Dir, M), score_animal(Plateau, (P,Dir), Q, S2), max(5,S2,S), !.

% animal à côté d'une montagne
score_animal(Plateau, (P,Dir), [(_, Lp)|Q], S) :- member(P,Lp), score_animal(Plateau, (P,Dir), Q, S2), max(3,S2,S), !.

% animal à côté d'une montagne mais dans la mauvaise direction
%score_animal((P,Dir), [(_, Lp)|Q], S) :- member(P,Lp), score_animal((P,Dir), Q, S2), S is 3 + S2, !. 

% sinon
score_animal(Plateau, P, [_|Q], S) :- score_animal(Plateau, P, Q, S).

score_animaux(_, [], _, 0) :- !.
score_animaux(P, [A|Q], M, S) :- score_animal(P, A, M, S2), score_animaux(P, Q,M,S3), S is S2+S3.

score_elep_rino(P, E,R,M, Se, Sr) :-
	findall((Montagne,Lp), (member(Montagne, M), cases_autour(Montagne, Lp)), Ms),
	score_animaux(P, E, Ms, Se),
	score_animaux(P, R, Ms, Sr).


%
% score_plateau(+Plateau, ?Score).
% plus le plateau est favorable, plus le score est haut.
%

% score maximal si le joueur courant gagne
score_plateau([E,R,M,J], 10000) :- gagnant([E,R,M,J], J), !.
% score minimal si le joueur courant perd
score_plateau([E,R,M,J], -10000) :- enemy(J,J2), gagnant([E,R,M,J], J2), !.

score_plateau([E,R,M,e], S) :-
	score_elep_rino([E,R,M,e], E,R,M,Se,Sr),
	S is Se - Sr,
	!.

score_plateau([E,R,M,r], S) :-
	score_elep_rino([E,R,M,e], E,R,M,Se,Sr),
	S is Sr - Se,
	!.

% sortir
score_coup(_P, (_,0,_), 10) :- !.


% pousser un truc bien
score_coup([E,R,M,e], (0,Pf,Dir), 4) :-
	prev_case(Pf, Dir, Prev),
	bonne_pousse_possible([[(Prev,Dir)|E],R,M,e], Prev, Dir), !.
score_coup([E,R,M,r], (0,Pf,Dir), 4) :-
	prev_case(Pf, Dir, Prev),
	bonne_pousse_possible([E,[(Prev,Dir)|R],M,r], Prev, Dir), !.
score_coup(P, (Pi,Pf,Dir), 4) :- Pi \= Pf, bonne_pousse_possible(P, Pi, Dir), !.
% aller sur une case où la poussée est possible
score_coup([E,R,M,e], (_,N,D), 5) :-
	next_case(N,D,Next),
	case_valid(Next),
	\+ case_vide([E,R,M,e], Next),
	bonne_pousse_possible([[(N,D)|E],R,M,e], N,D),
	!.
score_coup([E,R,M,r], (_,N,D), 5) :-
	next_case(N,D,Next),
	case_valid(Next),
	\+ case_vide([E,R,M,r], Next),
	bonne_pousse_possible([E,[(N,D)|R],M,r], N,D),
	!.
% aller sur une case face à une montagne
score_coup([_E,_R,M,_J], (_,N,D), 6) :-
	N \= 35, N \= 31,
	next_case(N,D,Next),
	member(Next, M),
	!.
% case vide près d'une montagne, bonne direction
%score_coup([E,R,M,J], (_,N,D), 5) :- case_vide([E,R,M,J], N), findall((Montagne,Lp), (member(Montagne, M), cases_autour(Montagne, Lp)), Ms), score_animal((N,D), Ms, S), S > 9,	!.

% case vide près d'une montagne, pas forcément bonne direction
%score_coup([E,R,M,J], (_,N,D), 6) :- case_vide([E,R,M,J], N), findall((Montagne,Lp), (member(Montagne, M), cases_autour(Montagne, Lp)), Ms), score_animal((N,D), Ms, S), S > 0, !.

% tourner
score_coup(_, (N,N,_), 9) :- !.

% aller sur case vide
score_coup(P, (X,N,_), 7) :- X \= 0, case_vide(P, N), !.
% entrer sur une bonne case
score_coup(P, (0,N,n), 6) :- case_vide(P,N), member(N, [12,13,14]), !.
score_coup(P, (0,N,s), 6) :- case_vide(P,N), member(N, [52,53,54]), !.
score_coup(P, (0,N,n), 6) :- liste_joueur(P,L), member((N,_),L), member(N, [12,13,14]), !.
score_coup(P, (0,N,s), 6) :- liste_joueur(P,L), member((N,_),L), member(N, [52,53,54]), !.
% entrer
score_coup(_P, (0,_,_), 8) :- !.
	
