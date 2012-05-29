
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
