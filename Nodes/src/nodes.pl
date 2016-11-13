:- use_module(library(lists)).
:- include('interface.pl').
:- include('utilities.pl').
:- use_module(library(random)).

:- volatile state/2, 
            lineTemp/1, 
            nodePosition/2, 
            player/1.

:- dynamic state/2, 
           lineTemp/1, 
           nodePosition/2,
           player/1.

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

unitPlayer(p1,unit1).
unitPlayer(p2,unit2).

nodePlayer(p1,node1).
nodePlayer(p2,node2).

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

validatePiece(p1, node1).
validatePiece(p1, unit1).
validatePiece(p2, node2).
validatePiece(p2, unit2).

node(node1).
node(node2).

nextPlayer(p1, p2).
nextPlayer(p2, p1).

findLinePiece([], _, _, _, _).

findLinePiece(List, NewRow, Row, Column, Piece) :-
        nth1(Column, List, Piece) ->
                Row is NewRow;
        true.
        
findPiece([], _, _, _, _).

findPiece([H|T], NewRow, Row, Column, Piece) :-
        findLinePiece(H, NewRow, Row, Column, Piece),
        NewTempRow is NewRow + 1,
        findPiece(T, NewTempRow, Row, Column, Piece).

checkPosition(Row, Column) :-
        Row > 0,
        Row < 3,
        Column > 3 - Row,
        Column < 11 - (3 - Row), 
        !.

checkPosition(Row, Column) :-
        Row > 2,
        Row < 8,
        Column > 1,
        Column < 11,
        !.

checkPosition(Row, Column) :-
        Row > 7,
        Row < 10,
        Column > Row - 6,
        Column < 10 + (8 - Row),
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

validateUnitMoveAux(_, _, _, _, Row, Column, Row, Column, _, _).

validateUnitMoveAux(Board, LineBoard, Piece, LinePiece, Row, Column, NewRow, NewColumn, TempRow, TempColumn) :-
         (Piece == 'empty', LinePiece == 'l1'; Piece == 'empty', LinePiece == 'l2') ->
                setCell(Row, Column, l, LineBoard, TempLineBoard),
                setCell(Row, Column, a, Board, NewBoard),
                validateUnitMove(TempRow, TempColumn, NewRow, NewColumn, NewBoard, TempLineBoard).

validateNodeMove(Row, Column, Row, Column, _, _) :- fail.

validateNodeMove(Row, Column, NewRow, NewColumn, Board, _) :-
        NewRow < Row + 2,
        NewRow > Row - 2,
        NewColumn < Column + 2,
        NewColumn > Column - 2,
        getPiece(Board, NewRow, NewColumn, empty).

validateUnitMove(Row, Column, Row, Column, _, _).

validateUnitMove(Row, Column, NewRow, NewColumn, Board, LineBoard) :-
        TempRow is Row - 1,
        getCommunication(LineBoard, Row, Column, LinePiece),
        getPiece(Board, TempRow, Column, Piece),
        validateUnitMoveAux(Board, LineBoard, Piece, LinePiece, Row, Column, NewRow, NewColumn, TempRow, Column);
        TempRow is Row + 1,
        getCommunication(LineBoard, Row, Column, LinePiece),
        getPiece(Board, TempRow, Column, Piece),
        validateUnitMoveAux(Board, LineBoard, Piece, LinePiece, Row, Column, NewRow, NewColumn, TempRow, Column);
        TempColumn is Column - 1,
        getCommunication(LineBoard, Row, Column, LinePiece),
        getPiece(Board, Row, TempColumn, Piece),
        validateUnitMoveAux(Board, LineBoard, Piece, LinePiece, Row, Column, NewRow, NewColumn, Row, TempColumn);     
        TempColumn is Column + 1,
        getCommunication(LineBoard, Row, Column, LinePiece),
        getPiece(Board, Row, TempColumn, Piece),
        validateUnitMoveAux(Board, LineBoard, Piece, LinePiece, Row, Column, NewRow, NewColumn, Row, TempColumn).

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
        (VRow == static, VColumn == dec) ->
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

lineAux(LineBoard, Row, Column, Piece, VRow, VColumn) :-
        getPiece(LineBoard, Row, Column, NewPiece),
        NewPiece == empty ->
                setCell(Row, Column, Piece, LineBoard, NewLineBoard),
                newLinePosition(Row, Column, NewRow, NewColumn, VRow, VColumn),
                assert(lineTemp(NewLineBoard)),
                line(Piece, NewRow, NewColumn, VRow, VColumn);
        assert(lineTemp(LineBoard)),
        newLinePosition(Row, Column, NewRow, NewColumn, VRow, VColumn),
        line(Piece, NewRow, NewColumn, VRow, VColumn).

line(_,0,0,_,_).

line(Piece, Row, Column, VRow, VColumn):-
        checkPosition(Row, Column) ->
                retract(lineTemp(LineBoard)),
                lineAux(LineBoard, Row, Column, Piece, VRow, VColumn);
        true.

updateLineBoardAux(Piece, Row, Column, LineBoard, NewLineBoard) :-
        assert(lineTemp(LineBoard)),
        NewRow1 is Row - 1,
        NewColumn1 is Column - 1,
        NewRow2 is Row + 1,
        NewColumn2 is Column + 1,
        /*UP row*/
        line(Piece, NewRow1, Column, dec, static),
        /*DOWN row*/
        line(Piece, NewRow2, Column, inc, static),
        /*LEFT row*/
        line(Piece, Row, NewColumn1, static, dec),
        /*RIGHT row*/
        line(Piece, Row, NewColumn2, static, inc),
        /*NORTHWEST row*/
        line(Piece, NewRow1, NewColumn1, dec, dec),
        /*SOUTHWEST row*/
        line(Piece, NewRow2, NewColumn1, inc, dec),
        /*NORTHEAST row*/
        line(Piece, NewRow1, NewColumn2, dec, inc),
        /*SOUTHEAST row*/
        line(Piece, NewRow2, NewColumn2, inc, inc),
        retract(lineTemp(NewLineBoard)).

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

finishMove(Piece, NewBoard, LineBoard) :-
        node(Piece) ->
                cleanBoard(LineBoard, p1, TempLineBoard),
                findPiece(NewBoard, 1, NewRow1, NewColumn1, node1),
                setCell(NewRow1, NewColumn1, l1, TempLineBoard, NewTempLineBoard2),
                updateLineBoard(p1, NewRow1, NewColumn1, NewTempLineBoard2, NewTempLineBoard3),
                displayBoard(NewBoard, NewTempLineBoard3),
                cleanBoard(NewTempLineBoard3, p2, NewTempLineBoard4), 
                findPiece(NewBoard, 1, NewRow2, NewColumn2, node2),
                setCell(NewRow2, NewColumn2, l2, NewTempLineBoard4, NewTempLineBoard),
                updateLineBoard(p2, NewRow2, NewColumn2, NewTempLineBoard, NewLineBoard),
                assert(state(NewBoard, NewLineBoard)),
                assert(nodePosition(NewBoard, NewLineBoard));
        assert(state(NewBoard, LineBoard)),
        !,
        fail.

moveNode(Row, Column, NewRow, NewColumn, Board, LineBoard, Piece) :-
        validateNodeMove(Row, Column, NewRow, NewColumn, Board, LineBoard) ->
                setCell(Row, Column, empty, Board, NewBoardTemp),
                setCell(NewRow, NewColumn, Piece, NewBoardTemp, NewBoard),
                finishMove(Piece, NewBoard, LineBoard);
        assert(state(Board, LineBoard)),
        fail.

moveUnit(Row, Column, NewRow, NewColumn, Board, LineBoard, Piece) :-
        validateUnitMove(Row, Column, NewRow, NewColumn, Board, LineBoard) ->
                setCell(Row, Column, empty, Board, NewBoardTemp),
                setCell(NewRow, NewColumn, Piece, NewBoardTemp, NewBoard),
                finishMove(Piece, NewBoard, LineBoard);
        assert(state(Board, LineBoard)),
        fail.

move(Piece, Row, Column, NewRow, NewColumn, Board, LineBoard, Piece) :-
        node(Piece) ->
                moveNode(Row, Column, NewRow, NewColumn, Board, LineBoard, Piece);
                moveUnit(Row, Column, NewRow, NewColumn, Board, LineBoard, Piece).
        

readMove(Player, Board, LineBoard, Piece, Row, Column) :-
        nl, displayBoard(Board, LineBoard),
        repeat,
                nl, write('Row: '),
                read(Row),
                write('Column: '),
                read(TempColumn),
                Column is TempColumn + 1,
                checkPosition(Row, Column),
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
                NewColumn is TempNewColumn + 1,
                checkPosition(NewRow, NewColumn),
                move(Piece, Row, Column, NewRow, NewColumn, Board, LineBoard, Piece),
                !.

finish(Player, Board):-
        nextPlayer(Player, Next),
        nodePlayer(Next, Node),
        findPiece(Board, 1, Row, Column, Node),
        unitPlayer(Player, Piece),
        NewRow1 is Row - 1,
        NewColumn1 is Column - 1,
        NewRow2 is Row + 1,
        NewColumn2 is Column + 1,
        getPiece(Board, NewRow1, Column, Piece),
        getPiece(Board, NewRow2, Column, Piece),
        getPiece(Board, Row, NewColumn1, Piece),
        getPiece(Board, Row, NewColumn2, Piece).

play(Type) :-
        Type =:= 1 -> playHH;
        Type =:= 2 -> playHC;
        Type =:= 3 -> playCC.

playHH :-
        repeat,
                retract(player(Player)), nl,
                write('Player: '), write(Player), nl, nl,
                nextMove(Player),
                retract(nodePosition(Board, NewLineBoard)),
                nextPlayer(Player, Next),
                assert(player(Next)),
                finish(Player, Board),
                displayBoard(Board, NewLineBoard),
                !.

nextHCMove(Player):-
        Player == p1 ->
        nextMove(Player);
        randPlay(Player).
        

playHC :- 
        repeat,
                retract(player(Player)), nl,
                write('Player: '), write(Player), nl, nl,
                nextHCMove(Player),
                retract(nodePosition(Board)),
                nextPlayer(Player, Next),
                assert(player(Next)),
                finish(Player, Board).

playCC :- 
        repeat,
                retract(player(Player)), nl,
                write('Player: '), write(Player), nl, nl,
                randPlay(Player),
                retract(nodePosition(Board)),
                nextPlayer(Player, Next),
                assert(player(Next)),
                finish(Player, Board).


match :-
        init(Type),
        play(Type).

moveRandUnit([],_,_,_).
                                                                    
moveRandUnit([L1|Ls],Piece,Board,LineBoard):-
        nth1(Row,L1,0),
        nth1(Column,L1,1),
        random(0,4,WillPlay),
        WillPlay > 2 ->
        findall([NewRow,NewColumn],validateUnitMove(Row, Column, NewRow, NewColumn, Board, LineBoard),Moves),
        length(Moves,LengthMoves),
        random(0,LengthMoves,Move),
        nth1(NewPos,Moves,Move),
        nth1(FinalRow,NewPos,0),
        nth1(FinalColumn,NewPos,1),
        moveUnit(Row, Column, FinalRow, FinalColumn, Board, LineBoard, Piece);
        moveRandUnit(Ls,Piece,Board,LineBoard).
        
randPlay(Player):- 
        retract(state(Board, LineBoard)),

        unitPlayer(Player,Piece),
        nodePlayer(Player,Node),
        
        findall([Row,Column],findPiece(Board, 1, Row, Column, Piece),Units),
        moveRandUnit(Units,Piece,Board,LineBoard),
        
        findPiece(Board, 1, NodeRow, NodeColumn, Node),
        findall([NewNodeRow,NewNodeColumn],validateUnitMove(NodeRow, NodeColumn, NewNodeRow, NewNodeColumn, Board, LineBoard),NodeMoves),
        length(NodeMoves,LengthNodeMoves),
        random(0,LengthNodeMoves,NodeMove),
        nth1(NewNodePos, NodeMoves, NodeMove),
        nth1(FinalNodeRow, NewNodePos, 0),
        nth1(FinalNodeColumn, NewNodePos, 1),
        moveNode(Row, Column, FinalNodeRow, FinalNodeColumn, Board, LineBoard, Piece),
        
        assert(state(Board, LineBoard)).


     
                
