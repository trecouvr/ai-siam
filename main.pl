%plateau_depart([[(13,n),(23,n),(33,n),(0,n),(0,e)],[(51,e),(52,e),(0,s),(0,w),(0,e)],[43,53,34],r]).
plateau_depart([[(0,n),(0,n),(0,w),(0,e),(0,e)],[(0,s),(0,s),(0,s),(0,w),(0,e)],[32,33,34],e]).

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

jouer_humain :-
	affiche_plateau_courant,
	demande_coup(X),
	get_plateau(P),
	jouer_coup(P, X, P2),
	clean_positions_plateau(P2,P3),
	set_plateau(P3).

jouer_ai :-
	affiche_plateau_courant,
	%write('entrez un truc\n'),read(_),
	get_plateau(P),
	alphabeta(4, P, X, S),
	write('Choose : '), write(X), write(' score : '), write(S), nl,
	jouer_coup(P, X, P2),
	clean_positions_plateau(P2,P3),
	set_plateau(P3).

jouer :-
	init_plateau,
	repeat,
	jouer_humain,
	get_plateau(P),
	gagnant(P,G),
	write('gagnant : '), write(G), write('\n'),
	!.

ai_vs_ai :-
	init_plateau,
	repeat,
	jouer_ai,
	get_plateau(P),
	gagnant(P,G),
	write('gagnant : '), write(G), write('\n'),
	!.

