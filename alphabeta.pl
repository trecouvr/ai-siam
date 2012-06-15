


:- include('evaluation.pl').

trunc(0, _, []) :- !.
trunc(_, [], []) :- !.
trunc(Lim, [X|Q], [X|Q2]) :- Lim2 is Lim-1, trunc(Lim2, Q, Q2).

sort_moves(Position, Moves, MovesSorted) :-
	findall(X-M, (member(M,Moves), score_coup(Position,M,X)), Moves2),
	keysort(Moves2, Moves3),
	trunc(10, Moves3, Moves4),
	findall(M2, (member(_-M2,Moves4)), MovesSorted).

alphabeta(D, P, Move, Value) :-
	infini(Inf),
	Alpha is 1-Inf,
	Beta is Inf-1,
	alphabeta(D,P,Alpha, Beta, Move, Value),
	!.

% alphabeta(+Plateau, +Alpha, +Beta, +Depth, ?Meilleur).
alphabeta(0,Position,_Alpha,_Beta,_NoMove,Value) :- 
	score_plateau(Position,Value).
alphabeta(D,Position,Alpha,Beta,Move,Value) :- 
	D > 0, 
	coups_possibles(Position, Moves),
	sort_moves(Position, Moves,MovesSorted),
	%write(MovesSorted),nl,
	Alpha1 is -Beta, % max/min
	Beta1 is -Alpha,
	D1 is D-1, 
	evaluate_and_choose(MovesSorted,Position,D1,Alpha1,Beta1,_,(Move,Value)).

evaluate_and_choose([Move|Moves],Position,D,Alpha,Beta,Record,BestMove) :-
	jouer_coup(Position,Move,Position1),
	alphabeta(D,Position1,Alpha,Beta,_OtherMove,Value),
	Value1 is -Value,
	cutoff(Move,Value1,D,Alpha,Beta,Moves,Position,Record,BestMove).
evaluate_and_choose([],_Position,_D,Alpha,_Beta,Move,(Move,Alpha)).

cutoff(Move,Value,_D,_Alpha,Beta,_Moves,_Position,_Record,(Move,Value)) :- 
	Value >= Beta,
	%write('cut !'),
	!.
cutoff(Move,Value,D,Alpha,Beta,Moves,Position,_Record,BestMove) :- 
	Alpha < Value, Value < Beta, !, 
	evaluate_and_choose(Moves,Position,D,Value,Beta,Move,BestMove).
cutoff(_Move,Value,D,Alpha,Beta,Moves,Position,Record,BestMove) :- 
	Value =< Alpha, !, 
	evaluate_and_choose(Moves,Position,D,Alpha,Beta,Record,BestMove).



%fonction ALPHABETA(P, A, B) /* A < B */
%   si P est une feuille alors
%       retourner la valeur de P
%   sinon
%       Meilleur = –INFINI
%       pour tout fils Pi de P faire
%           Val = -ALPHABETA(Pi,-B,-A)
%           si Val > Meilleur alors
%               Meilleur = Val
%               si Meilleur > A alors
%                      A = Meilleur
%                   si A ≥ B alors
%                       retourner Meilleur
%                   finsi
%               finsi
%           finsi 
%       finpour 
%       retourner Meilleur
%   finsi
%fin

