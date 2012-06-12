%plateau_depart([[(51,n),(52,s),(53,w),(54,e),(55,e)],[(11,n),(12,n),(13,s),(15,w),(14,e)],[32,33,34],e]).
plateau_depart([[(11,e),(22,s),(0,w),(0,e),(0,e)],[(0,n),(0,n),(0,s),(0,w),(0,e)],[12,33,34],e]).
%plateau_depart([[(23,n),(13,n),(0,w),(0,e),(0,e)],[(33,s),(43,s),(53,s),(0,w),(0,e)],[32,35,34],e]).

:- include('tools.pl').
:- include('affichage.pl').
:- include('pousse_possible.pl').
:- include('coups_possibles.pl').
:- include('jouer_coup.pl').
:- include('gagne.pl').
:- include('alphabeta.pl').


:- dynamic(plateau/1).
set_plateau(P) :- retractall(plateau(_)), asserta(plateau(P)).
get_plateau(P) :- plateau(P).
init_plateau :- plateau_depart(P), set_plateau(P).
affiche_plateau_courant :-
	plateau([E,R,M,J]),
	affiche_plateau([E,R,M,J]),
	write([E,R,M,J]), write('\n'),
	write('Player ('), write(J), write(') must play\n'), !.

demande_coup(X) :-
	repeat,
	get_plateau(P),
	write('Coup : '),
	read(X),
	coup_possible(P,X),
	!.


jouer :-
	init_plateau,
	repeat,
	affiche_plateau_courant,
	demande_coup(X),
	get_plateau(P),
	jouer_coup(P, X, P2),
	clean_positions_plateau(P2,P3),
	set_plateau(P3),
	gagnant(P3,G),
	write('gagnant : '), write(G), write('\n'),
	!.
