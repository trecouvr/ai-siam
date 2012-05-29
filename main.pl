%plateau_depart([[(51,n),(52,s),(53,w),(54,e),(55,e)],[(11,n),(12,n),(13,s),(15,w),(14,e)],[32,33,34],e]).
plateau_depart([[(11,e),(12,s),(0,w),(0,e),(0,e)],[(0,n),(0,n),(0,s),(0,w),(0,e)],[32,33,34],e]).
%plateau_depart([[(23,n),(13,n),(0,w),(0,e),(0,e)],[(33,s),(43,s),(53,s),(0,w),(0,e)],[32,35,34],e]).

element(X, [X|_]).
element(X, [_|R]) :- element(X,R).

affiche_plateau(Plateau):-write('+-----+-----+-----+-----+-----+'),nl,affiche_ligne(Plateau, 5, 1).

affiche_ligne(Plateau, I, K):-I>=K,affiche_case(Plateau, I, 1, 5),write('|'),nl,write('+-----+-----+-----+-----+-----+'),nl,NewI is I-1,affiche_ligne(Plateau, NewI, K).
affiche_ligne(_Plateau, I, K):- I < K.

affiche_case(Plateau, Ligne, I, K):-I=<K,NumCase is Ligne*10+I,affiche_contenu_case(Plateau, NumCase),tab(2),NewI is I+1,affiche_case(Plateau, Ligne, NewI, K).
affiche_case(_Plateau, _Ligne, I, K):- I > K.

affiche_contenu_case(_Plateau, _NumCase):-write('| '),fail.
affiche_contenu_case([_,_,M,_], NumCase):- member(NumCase, M),write(' M'),!.
affiche_contenu_case([_,R,_,_], NumCase):- member((NumCase, n), R),write('R↑'),!.
affiche_contenu_case([_,R,_,_], NumCase):- member((NumCase, s), R),write('R↓'),!.
affiche_contenu_case([_,R,_,_], NumCase):- member((NumCase, w), R),write('R←'),!.
affiche_contenu_case([_,R,_,_], NumCase):- member((NumCase, e), R),write('R→'),!.
affiche_contenu_case([E,_,_,_], NumCase):- member((NumCase, n), E),write('E↑'),!.
affiche_contenu_case([E,_,_,_], NumCase):- member((NumCase, s), E),write('E↓'),!.
affiche_contenu_case([E,_,_,_], NumCase):- member((NumCase, w), E),write('E←'),!.
affiche_contenu_case([E,_,_,_], NumCase):- member((NumCase, e), E),write('E→'),!.
affiche_contenu_case(_Plateau, _NumCase):-write('  ').

index(X,Y,N) :- var(X), x_y_case(X,Y,N), !.
index(X,Y,N) :- var(Y), x_y_case(X,Y,N), !.
index(X,Y,N) :- num_case(X,Y,N).
num_case(X,Y,N) :- N is X+Y*10.
x_y_case(X,Y,N) :- X is N mod 10, Y is N//10.

next_case(NumCase, e, NextCase) :- index(X,Y,NumCase), X2 is X+1, index(X2,Y,NextCase).
next_case(NumCase, w, NextCase) :- index(X,Y,NumCase), X2 is X-1, index(X2,Y,NextCase).
next_case(NumCase, s, NextCase) :- index(X,Y,NumCase), Y2 is Y-1, index(X,Y2,NextCase).
next_case(NumCase, n, NextCase) :- index(X,Y,NumCase), Y2 is Y+1, index(X,Y2,NextCase).

direction_opposee(e,w).
direction_opposee(s,n).
direction_opposee(w,e).
direction_opposee(n,s).

% case vide
case_vide([E,R,M,_], Position) :- \+member((Position,_), E), \+member((Position,_), R), \+member((Position,_), M).
case_vide(P,X,Y) :- index(X,Y,N), case_vide(N, P).

% est-ce que un animal est dehors ?
dehors([E,_,_,e], (0,O)) :- member((0,O),E).
dehors([_,R,_,r], (0,O)) :- member((0,O),R).

% est-ce que l'animal sur la positione est à moi
is_mine([E,_,_,e], NumCase, (NumCase, O)) :- member((NumCase,O), E).
is_mine([_,R,_,r], NumCase, (NumCase, O)) :- member((NumCase,O), R).

case_bordure(NumCase) :- member(NumCase, [11,12,13,14,15,25,35,45,55,54,53,52,51,41,31,21]).

% la case est dans le plateau ??
case_valid(NumCase) :- member(NumCase, [11,12,13,14,15,21,22,23,24,25,31,32,33,34,35,41,42,43,44,45,51,52,53,54,55]).

% l'orientation est valide
orientation_valid(O) :- member(O, [n,s,e,w]).

pousse_possible([E,R,M,_], NumCase, Direction) :-
	!,
	append(E,R,A),
	member((NumCase, Direction), A),
	force_masse(A,M, NumCase, Direction, Force, Masse),
	Force > 0,
	Force >= Masse.

% montagne
% force idem
% masse++
force_masse(A, M, NumCase, Direction, Force, Masse) :-
	member(NumCase, M),
	!,
	next_case(NumCase, Direction, NextCase),
	force_masse(A, M, NextCase, Direction, Force, Masse2),
	Masse is Masse2+1.

% éléphant ou rino dans la même direction
% force++
% masse idem
force_masse(A, M, NumCase, Direction, Force, Masse) :-
	member((NumCase, Direction), A),
	!,
	next_case(NumCase, Direction, NextCase),
	force_masse(A, M, NextCase, Direction, Force2, Masse),
	Force is Force2+1.

% éléphant ou rino dans la direction opposée
% force--
% masse idem
force_masse(A, M, NumCase, Direction, Force, Masse) :-
	direction_opposee(Direction, Opposee),
	member((NumCase, Opposee), A),
	!,
	next_case(NumCase, Direction, NextCase),
	force_masse(A, M, NextCase, Direction, Force2, Masse),
	Force is Force2-1.


% éléphant ou rino dans n'importe quelle position
% force idem
% masse idem
force_masse(A, M, NumCase, Direction, Force, Masse) :-
	member((NumCase, _), A),
	!,
	next_case(NumCase, Direction, NextCase),
	force_masse(A, M, NextCase, Direction, Force, Masse).
	

% vrai si force >= masse
force_masse(_, _, _, _, 0, 0).


% faire entrer une pièce
coup_possible(Plateau, (0, Position, O)) :-
	case_bordure(Position),
	orientation_valid(O),
	dehors(Plateau, _),
	case_vide(Plateau, Position).

% sortir une pièce
coup_possible(Plateau, (Position, 0, O)) :-
	case_bordure(Position),
	orientation_valid(O),
	is_mine(Plateau,Position,(Position,O)).

% changement orientation
coup_possible(Plateau, (P,P, O)) :-
	case_valid(P),
	orientation_valid(O),
	is_mine(Plateau,P,_).

% déplacement sur case vide
coup_possible(Plateau, (D,A,O)) :-
	orientation_valid(O),
	case_valid(D),
	case_valid(A),
	case_vide(Plateau, A),
	is_mine(Plateau, D, (D,O)),
	next_case(D,_,A).

% déplacement en poussant
coup_possible(Plateau, (Depart,Arrive,Direction)) :-
	orientation_valid(Direction),
	case_valid(Depart),
	case_valid(Arrive),
	\+ case_vide(Plateau,Arrive),
	next_case(Depart,Direction,Arrive),
	is_mine(Plateau, Depart, (Depart,Direction)),
	pousse_possible(Plateau, Depart, Direction).

demande_coup(X) :-
	repeat,
	write('Coup : '),
	read(X),
	coup_possible(X),
	!.
	
% :- dynamic(plateau/1).
% setplateau(P) :- retractall(plateau(_)), asserta(plateau(P)).
