:-use_module(library(lists)).

board([
        [null, null, roofL, empty, empty, empty, empty, empty, roofR, null, null],
        [null, roofL, empty, empty, empty, empty, empty, empty, empty, roofR, null],
        [roofLM, empty, empty, empty, empty, empty, empty, empty, empty, empty, null],
        [roofLM, empty, empty, empty, empty, empty, empty, empty, empty, empty, null],
        [roofLM, empty, empty, empty, empty, empty, empty, empty, empty, empty, null],
        [roofLM, empty, empty, empty, empty, empty, empty, empty, empty, empty, null],
        [roofLM, empty, empty, empty, empty, empty, empty, empty, empty, empty, null],
        [null, roofLM, empty, empty, empty, empty, empty, empty, empty, null, null],
        [null, null, roofLM, empty, empty, empty, empty, empty, null, null, null]
       ]).

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

init.
play.
finish.
showResult.


match :-
        init,
        repeat,
                play,
                finish,
        showResult.

