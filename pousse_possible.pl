
include('tools.pl').

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
