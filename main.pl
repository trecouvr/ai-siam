%plateau_depart([[(51,n),(52,s),(53,w),(54,e),(55,e)],[(11,n),(12,n),(13,s),(15,w),(14,e)],[32,33,34],e]).
plateau_depart([[(11,e),(12,s),(0,w),(0,e),(0,e)],[(0,n),(0,n),(0,s),(0,w),(0,e)],[32,33,34],e]).
%plateau_depart([[(23,n),(13,n),(0,w),(0,e),(0,e)],[(33,s),(43,s),(53,s),(0,w),(0,e)],[32,35,34],e]).

:- include('tools.pl').
:- include('affichage.pl').
:- include('pousse_possible.pl').
:- include('coups_possibles.pl').



demande_coup(X) :-
	repeat,
	write('Coup : '),
	read(X),
	coup_possible(X),
	!.
	
:- dynamic(plateau/1).
set_plateau(P) :- retractall(plateau(_)), asserta(plateau(P)).
affiche_plateau_courant :- plateau(X), affiche_plateau(X).

%jouer() :- repeat, demande_coup(X),
