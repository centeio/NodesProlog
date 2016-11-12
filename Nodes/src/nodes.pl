:- use_module(library(lists)).
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
        [roofLM, empty, l2, empty, empty, l2, empty, empty, l2, empty, null],
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
                board(Board),
                lineBoard(LineBoard),
                assert(state(Board, LineBoard)),
                assert(player(p1)),
        !.

finish :- 2 is 1.
showResult.

validatePiece(p1, node1).
validatePiece(p1, unit1).
validatePiece(p2, node2).
validatePiece(p2, unit2).

node(node1).
node(node2).

nextPlayer(p1, p2).
nextPlayer(p2, p1).

checkPosition(Row, Column) :-
        Row > 0,
        Row < 4,
        Column > 3 - Row,
        Column < 10 - (3 - Row), 
        !.

checkPosition(Row, Column) :-
        Row > 3,
        Row < 8,
        Column > 0,
        Column < 10,
        !.

checkPosition(Row, Column) :-
        Row > 7,
        Row < 10,
        Column > Row - 7,
        Column < 9 + (8 - Row),
        !.

getCommunication([H|_], 1, Column, Piece) :-
        Column > 0,
        Column < 10,
        nth1(Column, H, Piece), !.

getCommunication([_|T], Row, Column, Piece) :-
        Row > 1,
        TempRow is Row - 1,
        getCommunication(T, TempRow, Column, Piece), !.

getPiece([H|_], 1, Column, Piece) :-
        nth1(Column, H, Piece),
        !.

getPiece([_|T], Row, Column, Piece) :-
        Row > 1,
        TempRow is Row - 1,
        getPiece(T, TempRow, Column, Piece).

setCellLine(1, Piece, [_|T], [Piece|T]).

setCellLine(Column, Piece, [H|T], [H|NewT]) :-
        Column > 1,
        TempColumn is Column - 1,
        setCellLine(TempColumn, Piece, T, NewT).   

setCell(1, Column, Piece, [H|T], [NewHead|T]) :-
        setCellLine(Column, Piece, H, NewHead).
        
setCell(Row, Column, Piece, [H|T], [H|NewT]) :-
        Row > 1,
        TempRow is Row - 1, 
        setCell(TempRow, Column, Piece, T, NewT).

validateMove(Row, Column, Row, Column, _, _).

validateMove(Row, Column, NewRow, NewColumn, Board, LineBoard) :-
        TempRow is Row - 1,
        getCommunication(LineBoard, Row, Column, LinePiece),
        getPiece(Board, TempRow, Column, Piece),
        (Piece == 'empty', LinePiece == 'l1'; Piece == 'empty', LinePiece == 'l2') ->
                setCell(TempRow, Column, l, LineBoard, TempLineBoard),
                validateMove(TempRow, Column, NewRow, NewColumn, Board, TempLineBoard);
        TempRow is Row + 1,
        getCommunication(LineBoard, Row, Column, LinePiece),
        getPiece(Board, TempRow, Column, Piece),
        (Piece == 'empty', LinePiece == 'l1'; Piece == 'empty', LinePiece == 'l2') ->
                setCell(TempRow, Column, l, LineBoard, TempLineBoard),
                validateMove(TempRow, Column, NewRow, NewColumn, Board, TempLineBoard);
        TempColumn is Column - 1,
        getCommunication(LineBoard, Row, Column, LinePiece),
        getPiece(Board, Row, TempColumn, Piece),
        (Piece == 'empty', LinePiece == 'l1'; Piece == 'empty', LinePiece == 'l2') ->
                setCell(Row, TempColumn, l, LineBoard, TempLineBoard),
                validateMove(Row, TempColumn, NewRow, NewColumn, Board, TempLineBoard);        
        TempColumn is Column + 1,
        getCommunication(LineBoard, Row, Column, LinePiece),
        getPiece(Board, Row, TempColumn, Piece),
        (Piece == 'empty', LinePiece == 'l1'; Piece == 'empty', LinePiece == 'l2') ->
                setCell(Row, TempColumn, l, LineBoard, TempLineBoard),
                validateMove(Row, TempColumn, NewRow, NewColumn, Board, TempLineBoard).


newLinePosition(Row, Column, NewRow, NewColumn, VRow, VColumn) :-
        /*UP*/
        (VRow == dec, VColumn == static) ->
                NewRow is Row - 1,
                NewColumn is Column;
        /*DOWN*/
        (VRow == inc, VColumn == static) ->
                NewRow is Row + 1,
                NewColumn is Column;
        /*LEFT*/
        (VRow == static, VColumn == inc) ->
                NewRow is Row,
                NewColumn is Column + 1;
        /*RIGHT*/
        (VRow == inc, VColumn == static) ->
                NewRow is Row,
                NewColumn is Column - 1;
        /*NORTHWEST*/
        (VRow == dec, VColumn == dec) ->
                NewRow is Row - 1,
                NewColumn is Column - 1;
        /*SOUTHWEST*/
        (VRow == inc, VColumn == dec) ->
                NewRow is Row + 1,
                NewColumn is Column - 1;
        /*NORTHEAST*/
        (VRow == dec, VColumn == inc) ->
                NewRow is Row - 1,
                NewColumn is Column + 1;
        /*SOUTHEAST*/
        (VRow == inc, VColumn == inc) ->
                NewRow is Row + 1,
                NewColumn is Column + 1.

line(_,0,0,_,_,_,_).

line(Piece, Row, Column, LineBoard, NewLineBoard, VRow, VColumn):-
        checkPosition(Row, Column),
        setCell(Row, Column, Piece, LineBoard, NewLineBoard),
        newLinePosition(Row, Column, NewRow, NewColumn, VRow, VColumn),
        line(Piece, NewRow, NewColumn, LineBoard, NewLineBoard, VRow, VColumn).

updateLineBoardAux(Piece, Row, Column, LineBoard, NewLineBoard) :-
        /*UP row*/
        line(Piece, Row, Column, LineBoard, NewLineBoard, dec, static);
        /*DOWN row*/
        line(Piece, Row, Column, LineBoard, NewLineBoard, inc, static);
        /*LEFT row*/
        line(Piece, Row, Column, LineBoard, NewLineBoard, static, dec);
        /*RIGHT row*/
        line(Piece, Row, Column, LineBoard, NewLineBoard, static, inc);
        /*NORTHWEST row*/
        line(Piece, Row, Column, LineBoard, NewLineBoard, dec, dec);
        /*SOUTHWEST row*/
        line(Piece, Row, Column, LineBoard, NewLineBoard, inc, dec);
        /*NORTHEAST row*/
        line(Piece, Row, Column, LineBoard, NewLineBoard, dec, inc);
        /*SOUTHEAST row*/
        line(Piece, Row, Column, LineBoard, NewLineBoard, inc, inc);
        1 is 1.

updateLineBoard(p1, Row, Column, LineBoard, NewLineBoard) :-
        updateLineBoardAux(l1, Row, Column, LineBoard, NewLineBoard).

updateLineBoard(p2, Row, Column, LineBoard, NewLineBoard) :-
        updateLineBoardAux(l2, Row, Column, LineBoard, NewLineBoard).

cleanLine([], _, _). 

cleanLine([Piece|T], Piece, [empty|NewTail]) :-
        cleanLine(T, Piece, NewTail).

cleanLine([H|T], Piece, [H|NewTail]) :-
        cleanLine(T, Piece, NewTail).

cleanBoard([], _, _). 

cleanBoard([E1|Es], p1, [H|T]) :-
        cleanLine(E1, l1, H),
        cleanBoard(Es, p1, T).

cleanBoard([E1|Es], p2, [H|T]) :-
        cleanLine(E1, l2, H),
        cleanBoard(Es, p2, T).

finishMove(Player, Piece, Row, Column, NewBoard, LineBoard) :-
        node(Piece) ->
                cleanBoard(LineBoard, Player, NewTempLineBoard),
                updateLineBoard(Player, Row, Column, NewTempLineBoard, NewLineBoard),
                assert(state(NewBoard, NewLineBoard)),
                assert(nodePosition(NewBoard, Row, Column));
        assert(state(NewBoard, LineBoard)),
        !,
        fail.

move(Player, Row, Column, NewRow, NewColumn, Board, LineBoard, Piece) :-
        validateMove(Row, Column, NewRow, NewColumn, Board, LineBoard) ->
                setCell(Row, Column, empty, Board, NewBoardTemp),
                setCell(NewRow, NewColumn, Piece, NewBoardTemp, NewBoard),
                finishMove(Player, Piece, NewRow, NewColumn, NewBoard, LineBoard);
        assert(state(Board, LineBoard)),
        fail.

readMove(Player, Board, LineBoard, Piece, Row, Column) :-
        repeat,
                displayBoard(Board, LineBoard),
                write('Row: '),
                read(Row),
                write('Column: '),
                read(TempColumn),
                checkPosition(Row, TempColumn),
                Column is TempColumn + 1,
                getPiece(Board, Row, Column, Piece),
                validatePiece(Player, Piece),
                !.

nextMove(Player) :-
        repeat,
                retract(state(Board, LineBoard)),
                readMove(Player, Board, LineBoard, Piece, Row, Column), nl,
                write('Where to:'), nl,
                write('Row: '),
                read(NewRow),
                write('Column: '),
                read(TempNewColumn), nl,
                checkPosition(NewRow, TempNewColumn),
                NewColumn is TempNewColumn + 1,
                move(Player, Row, Column, NewRow, NewColumn, Board, LineBoard, Piece),
                !.

play(Type) :-
        Type =:= 1 -> playHH;
        Type =:= 2 -> playHC;
        Type =:= 3 -> playCC.

playHH :-
        repeat,
                retract(player(Player)), nl,
                write('Player: '), write(Player), nl, nl,
                nextMove(Player),
                retract(nodePosition(Board, NodeRow, NodeColumn)),
                nextPlayer(Player, Next),
                assert(player(Next)),
                finish(Board, NodeRow, NodeColumn).

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
        
        
