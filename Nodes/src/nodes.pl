:-use_module(library(lists)).
:- include('interface.pl').
:- include('utilities.pl').

board([
        [null, null, roofL, unit1, unit1, node1, unit1, unit1, roofR, null, null],
        [null, roofL, empty, empty, unit1, unit1, unit1, empty, empty, roofR, null],
        [roofLM, empty, empty, empty, empty, unit1, empty, empty, empty, empty, null],
        [roofLM, empty, empty, empty, empty, empty, empty, empty, empty, empty, null],
        [roofLM, empty, empty, empty, empty, empty, empty, empty, empty, empty, null],
        [roofLM, empty, empty, empty, empty, empty, empty, empty, empty, empty, null],
        [roofLM, empty, empty, empty, empty, unit2, empty, empty, empty, empty, null],
        [null, roofLM, empty, empty, unit2, unit2, unit2, empty, empty, null, null],
        [null, null, roofLM, unit2, unit2, node2, unit2, unit2, null, null, null]
       ]).

lineBoard([
        [null, null, roofL, l1, l1, l1, l1, l1, roofR, null, null],
        [null, roofL, empty, empty, l1, l1, l1, empty, empty, roofR, null],
        [roofLM, empty, empty, l1, empty, l1, empty, l1, empty, empty, null],
        [roofLM, empty, l1, empty, empty, l1, empty, empty, l1, empty, null],
        [roofLM, l1, empty, empty, empty, l1, empty, empty, empty, l1, null],
        [roofLM, empty, l2, empty, empty, empty, empty, empty, l2, empty, null],
        [roofLM, empty, empty, l2, empty, l2, empty, l2, empty, empty, null],
        [null, roofLM, empty, empty, l2, l2, l2, empty, empty, null, null],
        [null, null, roofLM, l2, l2, l2, l2, l2, null, null, null]
       ]).

player1(p1).
player2(p2).

init(Type) :-
        repeat,
                mainMenu,
                read(Type),
                (Type == 1; Type == 2; Type == 3),
                player1(Player),
                board(Board),
                lineBoard(LineBoard),
                assert(state(Player, Board, LineBoard)),
        !.
play.
finish.
showResult.

validatePiece(p1, node1).
validatePiece(p1, unit1).
validatePiece(p2, node2).
validatePiece(p2, unit2).

finishMove(node1).
finishMove(node2).

getPiece(Player, [H|_], 0, Column, Piece) :-
        nth1(Column, H, Piece),
        validatePiece(Player, Piece).

getPiece(Player, [_|T], Row, Column, Piece) :-
        Row > 0,
        Row is Row - 1,
        getPiece(Player, T, Row, Column, Piece).

validateMove(Row, Column, Row, Column, _).

validateMove(Row, Column, NewRow, NewColumn, LineBoard) :-
        TempRow is Row - 1,
        getCommunication(LineBoard, TempRow, Column, Piece),
        Piece == 'l' ->
                validateMove(TempRow, Column, NewRow, NewColumn, LineBoard),
        TempRow is Row + 1,
        getCommunication(LineBoard, TempRow, Column, Piece),
        Piece == 'l' ->
                validateMove(TempRow, Column, NewRow, NewColumn, LineBoard),
        TempColumn is Column - 1,
        getCommunication(LineBoard, Row, TempColumn, Piece),
        Piece == 'l' ->
                validateMove(TempRow, Column, NewRow, NewColumn, LineBoard),
        TempColumn is Column + 1,
        getCommunication(LineBoard, Row, TempColumn, Piece),
        Piece == 'l' ->
                validateMove(TempRow, Column, NewRow, NewColumn, LineBoard).

nextMove(Player, Board, NewBoard, LineBoard, NewLineBoard) :-
        repeat,
               repeat,
                        write('Row: '), nl,
                        read(Row),
                        write('Column: '), nl,
                        read(Column),
                        checkPosition(Row, Column),
                        getPiece(Player, Board, Row, Column, Piece),
                write('Where to'), nl,
                write('Row'), nl,
                read(NewRow),
                write('Column'), nl,
                read(NewColumn),
                validateMove(Row, Column, NewRow, NewColumn, LineBoard) ->
                        move(Row, Column, NewRow, NewColumn, Board, NewBoard, Piece),
                finishMove(Piece).

play(Type) :-
        Type =:= 1 -> playHH;
        Type =:= 2 -> playHC;
        Type =:= 3 -> playCC.

playHH :-
        repeat,
                retract(state(Player, Board, LineBoard)),
                displayBoard(board),
                nextMove(Player, Board, NewBoard, LineBoard, NewLineBoard),
                nextPlayer(Player, Next),
                assert(state(Next, NewBoard, NewLineBoard)),
                finish.

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