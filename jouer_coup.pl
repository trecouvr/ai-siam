
switch_joueur(e,r).
switch_joueur(r,e).

update_liste([], _, []) :- !.
% pour les animaux
update_liste([(Pi,_)|Q], (Pi,Pf,O), [(Pf,O)|Q]) :- !.
% pour les montagnes
update_liste([Pi|Q], (Pi,Pf), [Pf|Q]) :- !.
update_liste([X|Q], C, [X|Q2]) :- update_liste(Q,C,Q2).


% simple changement d'orientation
jouer_coup([E,R,M,J], (P,P,O), [E2,R2,M,J2]) :-
	update_liste(E, (P,P,O), E2),
	update_liste(R, (P,P,O), R2),
	switch_joueur(J,J2),
	!.

% faire entrer une pièce
jouer_coup([E,R,M,e], (0,P,O), [E2,R,M,r]) :-
	update_liste(E, (0,P,O), E2),
	!.
jouer_coup([E,R,M,r], (0,P,O), [E,R2,M,e]) :-
	update_liste(R, (0,P,O), R2),
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
