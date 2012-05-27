%plateau_depart([[(51,n),(52,s),(53,w),(54,e),(55,e)],[(11,n),(12,n),(13,s),(15,w),(14,e)],[32,33,34],e]).
%plateau_depart([[(0,n),(0,s),(0,w),(0,e),(0,e)],[(0,n),(0,n),(0,s),(0,w),(0,e)],[32,33,34],e]).
plateau_depart([[(23,n),(13,n),(0,w),(0,e),(0,e)],[(33,s),(43,s),(53,s),(0,w),(0,e)],[32,35,34],e]).

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
%pousse_possible([E,R,M,P], NumCase, Direction, Force, Masse) :- next_case(NumCase, Direction, NextCase), member(NextCase, M), !, Masse2 is Masse+1,	pousse_possible([E,R,M,P], NextCase, Direction, Force, Masse2).

% éléphant ou rino dans la même direction
% force++
% masse idem
force_masse(A, M, NumCase, Direction, Force, Masse) :-
	member((NumCase, Direction), A),
	!,
	next_case(NumCase, Direction, NextCase),
	force_masse(A, M, NextCase, Direction, Force2, Masse),
	Force is Force2+1.

%pousse_possible([E,R,M,P], NumCase, Direction, Force, Masse) :- next_case(NumCase, Direction, NextCase), append(E,R,L), member((NextCase,Direction), L), !, Force2 is Force+1, pousse_possible([E,R,M,P], NextCase, Direction, Force2, Masse).

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

%pousse_possible([E,R,M,P], NumCase, Direction, Force, Masse) :- next_case(NumCase, Direction, NextCase), direction_opposee(Direction, Opposee), append(E,R,L), member((NextCase,Opposee), L), !, Force2 is Force-1,	pousse_possible([E,R,M,P], NextCase, Direction, Force2, Masse).


% éléphant ou rino dans n'importe quelle position
% force idem
% masse idem
force_masse(A, M, NumCase, Direction, Force, Masse) :-
	member((NumCase, _), A),
	!,
	next_case(NumCase, Direction, NextCase),
	force_masse(A, M, NextCase, Direction, Force, Masse).
	
%pousse_possible([E,R,M,P], NumCase, Direction, Force, Masse) :- next_case(NumCase, Direction, NextCase), append(E,R,L), member((NextCase,_), L), !,	pousse_possible([E,R,M,P], NextCase, Direction, Force, Masse).


% case vide
% vrai si force >= masse
force_masse(_, _, _, _, 0, 0).


coup_possible(_).
demande_coup(X) :-
	repeat,
	write('Coup : '),
	read(X),
	coup_possible(X),
	!.
% :- dynamic(plateau/1).
% setplateau(P) :- retractall(plateau(_)), asserta(plateau(P)).
