


:- include('evaluation.pl').

trunc(0, _, []) :- !.
trunc(_, [], []) :- !.
trunc(Lim, [X|Q], [X|Q2]) :- Lim2 is Lim-1, trunc(Lim2, Q, Q2).

sort_moves(Position, Moves, MovesSorted) :-
	findall(X-M, (member(M,Moves), score_coup(Position,M,X)), Moves2),
	keysort(Moves2, Moves3),
	trunc(20, Moves3, Moves4),
	findall(M2, (member(_-M2,Moves4)), MovesSorted).

alphabeta(D, P, Move, Value) :-
	infini(Inf),
	Alpha is -1-Inf,
	Beta is Inf+1,
	alphabeta(D,P,Alpha, Beta, Move, Value),
	!.

% alphabeta(+Plateau, +Alpha, +Beta, +Depth, ?Meilleur).
alphabeta(0,Position,_Alpha,_Beta,_NoMove,Value) :- 
	score_plateau(Position,Value),
	%write('Score : '), write(Value), nl,
	!.
alphabeta(_D, Plateau, _Alpha, _Beta, _NoMove, 10000) :-
	joueur(Plateau,J), gagnant(Plateau, J),
	%write('Score : '), write(10000), nl,
	!.
alphabeta(_D, Plateau, _Alpha, _Beta, _NoMove, -10000) :-
	opposant(Plateau,J), gagnant(Plateau, J),
	%write('Score : '), write(-10000), nl,
	!.
alphabeta(D,Plateau,Alpha,Beta,Move,Value) :- 
	D > 0,
	clean_positions_plateau(Plateau,Plateau2),
	coups_possibles(Plateau2, Moves),
	sort_moves(Plateau2, Moves,MovesSorted),
	%write('MovesSorted : '), write(MovesSorted),nl,
	Alpha1 is -Beta, % max/min
	Beta1 is -Alpha,
	D1 is D-1, 
	evaluate_and_choose(MovesSorted,Plateau2,D1,Alpha1,Beta1,_,(Move,Value)).

evaluate_and_choose([Move|Moves],Position,D,Alpha,Beta,Record,BestMove) :-
	jouer_coup(Position,Move,Position1),
	%read(_), nl,nl,write('Coup joue : '), write(Move),nl, affiche_plateau(Position1), write(Position1),nl, write(D), write(' '), write(Move), write(' '), write([Alpha,Value,Beta]),nl,
	alphabeta(D,Position1,Alpha,Beta,_OtherMove,Value),
	Value1 is -Value,
	cutoff(Move,Value1,D,Alpha,Beta,Moves,Position,Record,BestMove).
evaluate_and_choose([],_Position,_D,Alpha,_Beta,Move,(Move,Alpha)).


cutoff(Move,Value,_D,_Alpha,Beta,_Moves,_Position,_Record,(Move,Value)) :-
	%write('cut1 '), write(Value), write(' >= '), write(Beta), nl,
	Value >= Beta,
	%write('cut !'), nl, write(Value), write(' >= '), write(Beta), nl,
	!.
cutoff(Move,Value,D,Alpha,Beta,Moves,Position,_Record,BestMove) :- 
	%write('cut2 '), write(Value), write(' IN '), write(aLPHA), write(' '), write(Beta), nl,
	Alpha < Value, Value < Beta, !, 
	evaluate_and_choose(Moves,Position,D,Value,Beta,Move,BestMove).
cutoff(_Move,Value,D,Alpha,Beta,Moves,Position,Record,BestMove) :- 
	%write('cut3 '), write(Value), write(' =< '), write(Alpha), nl,
	Value =< Alpha, !, 
	evaluate_and_choose(Moves,Position,D,Alpha,Beta,Record,BestMove).

