

entrer_piece([E,R,M,e], Depart, Dir) :- pousse_possible([[(Depart,Dir)|E],R,M,e], Depart, Dir).
entrer_piece([E,R,M,r], Depart, Dir) :- pousse_possible([E,[(Depart,Dir)|R],M,r], Depart, Dir).
case_entrer(Position, n, CaseEntrer) :-
	member(Position, [11,12,13,14,15]),
	next_case(Position, s, CaseEntrer).
case_entrer(Position, e, CaseEntrer) :-
	member(Position, [11,21,31,41,51]),
	next_case(Position, w, CaseEntrer).
case_entrer(Position, w, CaseEntrer) :-
	member(Position, [15,25,35,45,55]),
	next_case(Position, e, CaseEntrer).
case_entrer(Position, s, CaseEntrer) :-
	member(Position, [51,52,53,54,55]),
	next_case(Position, n, CaseEntrer).

% faire entrer une pièce sur une case vide
coup_possible(Plateau, (0, Position, O)) :-
	case_bordure(Position),
	orientation_valid(O),
	check_dehors(Plateau),
	case_vide(Plateau, Position).

coup_possible(Plateau, (0, Position, O)) :-
	case_bordure(Position),
	orientation_valid(O),
	check_dehors(Plateau),
	\+ case_vide(Plateau, Position),
	case_entrer(Position, O, CaseEntrer),
	entrer_piece(Plateau, CaseEntrer, O).
	
% sortir une pièce
coup_possible(Plateau, (Position, 0, O)) :-
	case_bordure(Position),
	orientation_valid(O),
	is_mine(Plateau,Position,(Position,O)).

% changement orientation
coup_possible(Plateau, (P, P, O)) :-
	case_valid(P),
	orientation_valid(O),
	is_mine(Plateau,P,(P,O2)),
	O2 \= O.

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


coups_possibles(Plateau, L) :-
	findall(C, coup_possible(Plateau,C), L), !.


