translateContent(null, '    ').
translateContent(empty, '   |').
translateContent(unit1, ' x |').
translateContent(unit2, ' o |').
translateContent(node1, ' 1 |').
translateContent(node2, ' 2 |').
translateContent(roofL, '    |').
translateContent(roofLM, '    |').
translateContent(roofR, '').

translateBottom(null, '    ').
translateBottom(X, '___|') :-
        member(X, [empty, unit1, unit2, node1, node2]).
translateBottom(roofL, ' ___|').
translateBottom(roofLM, '    |').
translateBottom(roofR, '___').

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

displayBoardAux([]) :-
        displayLine([]).

displayBoardAux([L1|Ls]):-
        displayLine(L1),
        nl,
        displayLineBottom(L1),
        nl,
        displayBoardAux(Ls).

displayBoard(X) :-
        write('             ___ ___ ___ ___ ___'),
        nl,
        displayBoardAux(X).

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