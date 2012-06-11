
update_liste([], _, []) :- !.
update_liste([(Pi,_)|Q], (Pi,Pf,O), [(Pf,O)|Q]) :- !.
update_liste([X|Q], C, [X|Q2]) :- update_liste(Q,C,Q2).

% simple changement d'orientation
jouer_coup([E,R,M,J], (P,P,O), [E2,R2,M,J]) :- update_liste(E, (P,P,O), E2), update_liste(R, (P,P,O), R2), !.

% déplacement sur case vide
jouer_coup([E,R,M,J], (Pi,Pf,O), [E2,R2,M,J]) :-
	case_vide([E,R,M,J], Pf),
	update_liste(E, (Pi,Pf,O), E2),
	update_liste(R, (Pi,Pf,O), R2),
	!.
