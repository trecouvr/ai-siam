

update_liste([], _, []) :- !.
% pour les animaux
update_liste([(Pi,_)|Q], (Pi,Pf,O), [(Pf,O)|Q]) :- !.
% pour les montagnes
update_liste([Pi|Q], (Pi,Pf), [Pf|Q]) :- !.
update_liste([X|Q], C, [X|Q2]) :- update_liste(Q,C,Q2).

update_player_liste([E,R,M,e], NewPos, [E2,R,M,e]) :- update_liste(E, NewPos, E2).
update_player_liste([E,R,M,r], NewPos, [E,R2,M,r]) :- update_liste(R, NewPos, R2).

% simple changement d'orientation
jouer_coup([E,R,M,J], (P,P,O), [E2,R2,M,J2]) :-
	update_player_liste([E,R,M,J], (P,P,O), [E2,R2,M,J]),
	switch_joueur(J,J2),
	!.

% faire sortir une pièce
jouer_coup([E,R,M,J], (P,0,O), [E2,R2,M,J2]) :-
	update_player_liste([E,R,M,J], (P,0,O), [E2,R2,M,J]),
	switch_joueur(J,J2),
	!.

% déplacement case vide
jouer_coup([E,R,M,J], (Pi,Pf,O), [E2,R2,M,J2]) :-
	case_vide([E,R,M,e], Pf),
	update_player_liste([E,R,M,J], (Pi,Pf,O), [E2,R2,M,J]),
	switch_joueur(J,J2),
	!.

% faire entrer une pièce sur case occuppée
jouer_coup([E,R,M,J], (0,P,O), [E3,R3,M3,J2]) :-
	prev_case(P, O, Prev),
	update_player_liste([E,R,M,J], (0,Prev,O), [E2,R2,M,J]),
	deplacer_obj([E2,R2,M,J], (Prev,P,O), [E3,R3,M3,J]),
	switch_joueur(J,J2),
	!.

% déplacement sur case occupée
% il faut déplacer tout le monde
% du coup on effectue un jouer coup pour tous les objets sur la ligne
jouer_coup([E,R,M,J], (Pi,Pf,Dir), [E2,R2,M2,J2]) :-
	deplacer_obj([E,R,M,J], (Pi,Pf,Dir), [E2,R2,M2,J]),
	switch_joueur(J,J2),
	!.

deplacer_obj(P, (Pi,Pf,Dir), P3) :-
	\+ case_vide(P, Pf),
	%write('hey'), write(Pi), write(' '), write(Pf), write(' '), write('\n'),
	next_case(Pf, Dir, NextCase),	% calcul de la prochaine case pour l'objet qui nous fait chier
	deplacer_obj(P, (Pf,NextCase,Dir), P2),
	deplacer_obj(P2, (Pi,Pf,Dir), P3).
deplacer_obj([E,R,M,J], (Pi,Pf,_), [E2,R,M,J]) :- member((Pi,O), E), update_liste(E, (Pi,Pf,O), E2), !.
deplacer_obj([E,R,M,J], (Pi,Pf,_), [E,R2,M,J]) :- member((Pi,O), R), update_liste(R, (Pi,Pf,O), R2), !.
deplacer_obj([E,R,M,J], (Pi,Pf,_), [E,R,M2,J]) :- update_liste(M, (Pi,Pf), M2), !.	
