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

nextPlayer(p1, p2).
nextPlayer(p2, p1).

checkPosition(Row, Column) :-
        Row > 0,
        Row < 4,
        Column > 3 - Row,
        Column < 10 - (3 - Row).

checkPosition(Row, Column) :-
        Row > 3,
        Row < 8,
        Column > 0,
        Column < 10.

checkPosition(Row, Column) :-
        Row > 7,
        Row < 10,
        Column > Row - 7,
        Column < 9 + (8 - Row).

getCommunication([H|_], 0, Column, Piece) :-
        nth1(Column, H, Piece).

getCommunication([_|T], Row, Column, Piece) :-
        Row > 0,
        Row is Row - 1,
        getCommunication(T, Row, Column, Piece).

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
        (Piece == 'l1'; Piece == 'l2') ->
                validateMove(TempRow, Column, NewRow, NewColumn, LineBoard);
        TempRow is Row + 1,
        getCommunication(LineBoard, TempRow, Column, Piece),
        (Piece == 'l1'; Piece == 'l2') ->
                validateMove(TempRow, Column, NewRow, NewColumn, LineBoard);
        TempColumn is Column - 1,
        getCommunication(LineBoard, Row, TempColumn, Piece),
        (Piece == 'l1'; Piece == 'l2') ->
                validateMove(Row, TempColumn, NewRow, NewColumn, LineBoard);
        TempColumn is Column + 1,
        getCommunication(LineBoard, Row, TempColumn, Piece),
        (Piece == 'l1'; Piece == 'l2') ->
                validateMove(Row, TempColumn, NewRow, NewColumn, LineBoard).

setCellLine(0, Piece, [_|T], [Piece|T]).

setCellLine(Column, Piece, [H|T], [H|NewT]) :-
        Column > 0,
        Column is Column - 1,
        setCellLine(Column, Piece, T, NewT).   

setCell(0, Column, Piece, [H|T], [NewHead|T]) :-
        setCellLine(Column, Piece, H, NewHead).
        
setCell(Row, Column, Piece, [H|T], [H|NewT]) :-
        Row > 0,
        Row is Row - 1, 
        setCell(Row, Column, Piece, T, NewT).

move(Row, Column, NewRow, NewColumn, Board, NewBoard, Piece) :-
        NewBoard is Board,
        setCell(Row, Column, empty, Board, NewBoard),
        setCell(NewRow, NewColumn, Piece, Board, NewBoard).

nextMove(Player, Board, NewBoard, LineBoard, NewLineBoard) :-
        repeat,
               repeat,
                        write('Row: '),
                        read(Row), nl,
                        write('Column: '),
                        read(Column), nl,
                        checkPosition(Row, Column),
                        getPiece(Player, Board, Row, Column, Piece),
                write('Where to'), nl,
                write('Row'), nl,
                read(NewRow),
                write('Column'), nl,
                read(NewColumn),
                validateMove(Row, Column, NewRow, NewColumn, LineBoard) ->
                        move(Row, Column, NewRow, NewColumn, Board, NewBoard, Piece),
                finishMove(Piece),
         updateLineBoard(LineBoard, NewLineBoard).

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


diagonals(Piece,Row,Column,Board):-
        diagonal(Piece,Board,Row,Column,inc,inc);
        diagonal(Piece,Board,Row,Column,inc,dec);
        diagonal(Piece,Board,Row,Column,dec,inc);
        diagonal(Piece,Board,Row,Column,dec,dec).

diagonal(_,_,0,_,_,_).

diagonal(Piece,Board,NewBoard,Row,Column,VRow,VColumn):-
        Row>0,
        Row<10,
        Column>0,
        Column<10,
        setPiece(Row,Column,Piece,Board,NewBoard),
        VRow == inc ->
        Row is Row + 1;
        VRow == dec ->
        Row is Row - 1;
        VColumn == inc ->
        Column is Column + 1;
        VColumn == inc ->
        Column is Column + 1,
        diagonal(Piece,Board,NewBoard,Row,Column,VRow,VColumn).

