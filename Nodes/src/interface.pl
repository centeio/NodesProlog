translateContent(null, '    ').
translateContent(empty, '   |').
translateContent(unit1, ' x |').
translateContent(unit2, ' o |').
translateContent(node1, ' 1 |').
translateContent(node2, ' 2 |').
translateContent(a, ' a |').
translateContent(roofL, '    |').
translateContent(roofLM, '    |').
translateContent(roofR, '    ').
translateContent(l, ' l |').
translateContent(k, ' k |').

translateBottom(null, '    ').
translateBottom(X, '___|') :-
        member(X, [empty, unit1, unit2, node1, node2, l1, l2, a, l, k]).
translateBottom(roofL, ' ___|').
translateBottom(roofLM, '    |').
translateBottom(roofR, '___ ').

displayLineBottom([]) :-
        write('').

displayLineBottom([E1|Es]) :-
        translateBottom(E1, V),
        write(V),
        displayLineBottom(Es).

displayLine([]) :-
        write('').

displayLine([E1|Es]) :-
        translateContent(E1, V),
        write(V),
        displayLine(Es).    

displayBoardAux([], [], _) :-
        displayLine([]).

displayBoardAux([L1|Ls], [K1|Ks], Counter):-
        write(Counter),
        displayLine(L1),
        write(Counter),
        displayLine(K1),
        nl,
        write(' '),
        displayLineBottom(L1),
        write(' '),
        displayLineBottom(K1),
        nl,
        Next is Counter + 1,
        displayBoardAux(Ls, Ks, Next).

displayBoard(X, Y) :-
        write('       1   2   3   4   5   6   7   8   9             1   2   3   4   5   6   7   8   9  '), nl, 
        write('              ___ ___ ___ ___ ___                           ___ ___ ___ ___ ___'),
        nl,
        displayBoardAux(X, Y, 1).

mainMenu :-
        write('****************************************'), nl,
        write('****************************************'), nl,
        write('**                                    **'), nl,
        write('**                                    **'), nl,
        write('**          Welcome to Nodes          **'), nl,
        write('**                                    **'), nl,
        write('**                                    **'), nl,
        write('****************************************'), nl,
        write('****************************************'), nl,
        write('**                                    **'), nl,
        write('**                                    **'), nl,
        write('**             Main menu              **'), nl,
        write('**                                    **'), nl,
        write('**        1 - Player vs Player        **'), nl,
        write('**       2 - Player vs Computer       **'), nl,
        write('**      3 - Computer vs Computer      **'), nl,
        write('**                                    **'), nl,
        write('**                                    **'), nl,
        write('****************************************'), nl,
        write('****************************************'), nl.

levelMenu :-
        write('****************************************'), nl,
        write('****************************************'), nl,
        write('**                                    **'), nl,
        write('**                                    **'), nl,
        write('**      2 - Player vs Computer        **'), nl,
        write('**                                    **'), nl,
        write('**                                    **'), nl,
        write('****************************************'), nl,
        write('****************************************'), nl,
        write('**                                    **'), nl,
        write('**                                    **'), nl,
        write('**               Level                **'), nl,
        write('**                                    **'), nl,
        write('**             1 - Easy               **'), nl,
        write('**             2 - Hard               **'), nl,
        write('**                                    **'), nl,
        write('**                                    **'), nl,
        write('**                                    **'), nl,
        write('****************************************'), nl,
        write('****************************************'), nl.