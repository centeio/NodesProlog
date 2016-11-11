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
line1(l1).
line2(l2).

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

finishMove(node1).
finishMove(node2).

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

move(Row, Column, NewRow, NewColumn, Board, NewBoard, LineBoard, Piece) :-
        validateMove(Row, Column, NewRow, NewColumn, Board, LineBoard) ->
                setCell(Row, Column, empty, Board, NewBoardTemp),
                setCell(NewRow, NewColumn, Piece, NewBoardTemp, NewBoard);
        assert(state(Board, LineBoard)).

updateLineBoard(Player, Piece, Row, Column, Board, NewBoard) :-
        Player == p1 ->
                line1(Piece);
                line2(Piece),
        diagonal(Piece, Board, NewBoard, Row, Column, inc, inc);
        diagonal(Piece, Board, NewBoard, Row, Column, inc, dec);
        diagonal(Piece, Board, NewBoard, Row, Column, dec, inc);
        diagonal(Piece, Board, NewBoard, Row, Column, dec, dec).

diagonal(_, _, 0, _, _, _).

diagonal(Piece, Board, NewBoard, Row, Column, VRow, VColumn):-
        Row > 0,
        Row < 10,
        Column > 0,
        Column < 10,
        setCell(Row, Column, Piece, Board, NewBoard),
        VRow == inc ->
        Row is Row + 1;
        VRow == dec ->
        Row is Row - 1;
        VColumn == inc ->
        Column is Column + 1;
        VColumn == inc ->
        Column is Column + 1,
        diagonal(Piece, Board, NewBoard, Row, Column, VRow, VColumn).

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
                move(Row, Column, NewRow, NewColumn, Board, NewBoard, LineBoard, Piece),
                assert(state(NewBoard, LineBoard)),
                finishMove(Piece),
                /*updateLineBoard(Player, NewRow, NewColumn, LineBoard, NewLineBoard), */
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
                nextPlayer(Player, Next),
                assert(player(Next)),
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

cleanBoard([E1|Es],Piece,Row,Column,[H|T]) :-
        cleanLine(E1,Piece,Row,Column,H),
        cleanBoard(Es,Piece,Row+1,Column,T).

cleanLine([Piece|T], Piece, [empty|NewTail]) :-
        cleanLine(T, Piece, NewTail).

/*cleanLine([E1|Es],Piece,Row,Column,Board,NewBoard):-
        E1 == Piece ->
        E1 is empty,
        cleanLine(Es,Piece,Row,Column-1,Board,NewBoard).*/
        
        
