

% faire entrer une pièce
coup_possible(Plateau, (0, Position, O)) :-
	case_bordure(Position),
	orientation_valid(O),
	check_dehors(Plateau),
	case_vide(Plateau, Position).

% sortir une pièce
coup_possible(Plateau, (Position, 0, O)) :-
	case_bordure(Position),
	orientation_valid(O),
	is_mine(Plateau,Position,(Position,O)).

% changement orientation
coup_possible(Plateau, (P, P, O)) :-
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


coups_possibles(Plateau, L) :- findall(C, coup_possible(Plateau,C), L), !.


