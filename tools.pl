
index(X,Y,N) :- var(X), x_y_case(X,Y,N), !.
index(X,Y,N) :- var(Y), x_y_case(X,Y,N), !.
index(X,Y,N) :- num_case(X,Y,N).
num_case(X,Y,N) :- N is X+Y*10.
x_y_case(X,Y,N) :- X is N mod 10, Y is N//10.

next_case(NumCase, e, NextCase) :- index(X,Y,NumCase), X2 is X+1, clean_position((X2,Y),(X3,Y3)), index(X3,Y3,NextCase), !.
next_case(NumCase, w, NextCase) :- index(X,Y,NumCase), X2 is X-1, clean_position((X2,Y),(X3,Y3)), index(X3,Y3,NextCase), !.
next_case(NumCase, s, NextCase) :- index(X,Y,NumCase), Y2 is Y-1, clean_position((X,Y2),(X3,Y3)), index(X3,Y3,NextCase), !.
next_case(NumCase, n, NextCase) :- index(X,Y,NumCase), Y2 is Y+1, clean_position((X,Y2),(X3,Y3)), index(X3,Y3,NextCase), !.

clean_position((X,_),(0,0)) :- X =< 0, !.
clean_position((X,_),(0,0)) :- X > 5, !.
clean_position((_,Y),(0,0)) :- Y =< 0, !.
clean_position((_,Y),(0,0)) :- Y > 5, !.
clean_position(P,P).

direction_opposee(e,w).
direction_opposee(s,n).
direction_opposee(w,e).
direction_opposee(n,s).

% case vide
case_vide(_, 0) :- !.
case_vide([E,R,M,_], Position) :- \+member((Position,_), E), \+member((Position,_), R), \+member(Position, M).
case_vide(P,X,Y) :- index(X,Y,N), case_vide(N, P).

% est-ce que un animal est dehors ?
dehors([E,_,_,e], (0,O)) :- member((0,O),E).
dehors([_,R,_,r], (0,O)) :- member((0,O),R).

% est-ce que l'animal sur la positione est à moi
is_mine([E,_,_,e], NumCase, (NumCase, O)) :- member((NumCase,O), E).
is_mine([_,R,_,r], NumCase, (NumCase, O)) :- member((NumCase,O), R).

case_bordure(NumCase) :- member(NumCase, [11,12,13,14,15,25,35,45,55,54,53,52,51,41,31,21]).

% la case est dans le plateau ??
case_valid(NumCase) :- member(NumCase, [11,12,13,14,15,21,22,23,24,25,31,32,33,34,35,41,42,43,44,45,51,52,53,54,55]).

% l'orientation est valide
orientation_valid(O) :- member(O, [n,s,e,w]).

