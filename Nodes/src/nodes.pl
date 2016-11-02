:-use_module(library(lists)).
:- include('interface.pl').
:- include('utilities.pl').

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


translateChoice(1, hh).
translateChoice(2, hc).
translateChoice(3, cc).

init(Type) :-
        repeat,
                mainMenu,
                read(Choice),
                (Choice =:= 1; Choice =:= 2; Choice =:= 3),
        !,
        translateChoice(Choice, Type).
play.
finish.
showResult.

play(Type) :-
        type(Type),
        Type == hh -> playHH;
        Type == hc -> playHC;
        Type == cc -> playCC.

playHH :-
        write('HH').

playHC :- 
        write('HC').

playCC :- 
        write('CC').


match :-
        init(Type),
        repeat,
                play(Type),
                finish,
        showResult.