:- use_module(library(lists)).
:- include('interface.pl').
:- include('utilities.pl').
:- use_module(library(random)).

:- volatile state/2, 
            lineTemp/1, 
            nodePosition/2, 
            player/1,
            randlist/1,
            validateComputerList/1.

:- dynamic state/2, 
           lineTemp/1, 
           nodePosition/2,
           player/1,
           randlist/1,
           validateComputerList/1.

/*Initial Play Board: roofs are margins, null are out of the board, empty are empty, unit1 and node1 are Units and Nodes
of Player 1 and similar happens to Player 2*/

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

/*Board that contains where the Communication Lines are (signed with l)*/

lineBoard([
        [null, null, roofL, l, l, l, l, l, roofR, null, null],
        [null, roofL, empty, empty, l, l, l, empty, empty, roofR, null],
        [roofLM, empty, empty, l, empty, l, empty, l, empty, empty, null],
        [roofLM, empty, l, empty, empty, l, empty, empty, l, empty, null],
        [roofLM, l, empty, empty, empty, l, empty, empty, empty, l, null],
        [roofLM, empty, l, empty, empty, l, empty, empty, l, empty, null],
        [roofLM, empty, empty, l, empty, l, empty, l, empty, empty, null],
        [null, roofLM, empty, empty, l, l, l, empty, empty, null, null],
        [null, null, roofLM, l, l, l, l, l, null, null, null]
       ]).

/*players*/
player1(p1).
player2(p2).

/*which units belong to each player*/
unitPlayer(p1,unit1).
unitPlayer(p2,unit2).

/*which node belong to each player*/
nodePlayer(p1,node1).
nodePlayer(p2,node2).

/*starting function: calls the menu and gets the type of game. Also initializes both boards.*/
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

/*
choose(1,playHC).
choose(2,hard).

chooseLevel :-
        repeat,
                levelMenu,
                read(Type),
                choose(Type,Func),
                Func,
        !.*/

/*checks if the piece belong to the player*/
validatePiece(p1, node1).
validatePiece(p1, unit1).
validatePiece(p2, node2).
validatePiece(p2, unit2).

/*checks if it is a node*/
node(node1).
node(node2).

/*used to switch players*/
nextPlayer(p1, p2).
nextPlayer(p2, p1).

/*findUnits will find all the positions of units (unit1 OR unit2) on the board*/

findUnitsAux([], _, _, _).

findUnitsAux([Piece|T], Row, Column, Piece) :-
        retract(randlist(List)),
        append(List, [[Row, Column]], NewList),
        assert(randlist(NewList)),
        NewColumnCounter is Column + 1,
        findUnitsAux(T, Row, NewColumnCounter, Piece).

findUnitsAux([_|T], RowCounter, ColumnCounter, Piece) :-
        NewColumnCounter is ColumnCounter + 1,
        findUnitsAux(T, RowCounter, NewColumnCounter, Piece).

findUnits([], _, _, _).

findUnits([H|T], RowCounter, ColumnCounter, Piece) :-
        findUnitsAux(H, RowCounter, ColumnCounter, Piece),
        NewRowCounter is RowCounter + 1,
        findUnits(T, NewRowCounter, 1, Piece).

/*findNode will find the position of a node (node1 OR node2) on the board*/

findNodeAux([], _, _, _, _).

findNodeAux(List, NewRow, Row, Column, Piece) :-
        nth1(Column, List, Piece) ->
                Row is NewRow;
        true.
        
findNode([], _, _, _, _).

findNode([H|T], NewRow, Row, Column, Piece) :-
        findNodeAux(H, NewRow, Row, Column, Piece),
        NewTempRow is NewRow + 1,
        findNode(T, NewTempRow, Row, Column, Piece).

/*checks if the position is inside the board*/
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
/*gets piece in LineBoard*/
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

/*sets cell in a position of a line*/
setCellLine(1, Piece, [_|T], [Piece|T]).

setCellLine(Column, Piece, [H|T], [H|NewT]) :-
        Column > 1,
        TempColumn is Column - 1,
        setCellLine(TempColumn, Piece, T, NewT).   

/*sets cell in a board*/
setCell(1, Column, Piece, [H|T], [NewHead|T]) :-
        setCellLine(Column, Piece, H, NewHead).
        
setCell(Row, Column, Piece, [H|T], [H|NewT]) :-
        Row > 1,
        TempRow is Row - 1, 
        setCell(TempRow, Column, Piece, T, NewT).

/*checks if node can move to desired position*/
validateNodeMove(Row, Column, Row, Column, _) :- fail.

validateNodeMove(Row, Column, NewRow, NewColumn, Board) :-
        NewRow is Row,
        NewColumn < Column + 2,
        NewColumn > Column - 2,
        getPiece(Board, NewRow, NewColumn, empty);
        NewRow < Row + 2,
        NewRow > Row - 2,
        NewColumn is Column,
        getPiece(Board, NewRow, NewColumn, empty).

/*checks if unit can mov to desired position*/
validateUnitMoveAux(_, _, _, _, Row, Column, Row, Column, _, _).

validateUnitMoveAux(Board, LineBoard, Piece, LinePiece, Row, Column, NewRow, NewColumn, TempRow, TempColumn) :-
         (Piece == 'empty', LinePiece == 'l') ->
                setCell(Row, Column, k, LineBoard, TempLineBoard),
                setCell(Row, Column, a, Board, NewBoard),
                validateUnitMove(TempRow, TempColumn, NewRow, NewColumn, NewBoard, TempLineBoard).

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


/*adds a possible position to a list*/
validateComputerUnitMoveAux(Row, Column) :-
        retract(validateComputerList(List)),
        append(List, [[Row, Column]], NewList),
        assert(validateComputerList(NewList)).

/*finds possible positions to where units can move. It is divided in four main different functions so it cannot cycle*/

validateComputerUnitMoveRIGHTAux(0, _, _, _).
validateComputerUnitMoveRIGHTAux(_, 0, _, _).
validateComputerUnitMoveRIGHTAux(10, _, _, _).
validateComputerUnitMoveRIGHTAux(_, 10, _, _).

validateComputerUnitMoveLEFTAux(0, _, _, _).
validateComputerUnitMoveLEFTAux(_, 0, _, _).
validateComputerUnitMoveLEFTAux(10, _, _, _).
validateComputerUnitMoveLEFTAux(_, 10, _, _).

validateComputerUnitMoveUPAux(0, _, _, _).
validateComputerUnitMoveUPAux(_, 0, _, _).
validateComputerUnitMoveUPAux(10, _, _, _).
validateComputerUnitMoveUPAux(_, 10, _, _).

validateComputerUnitMoveDOWNAux(0, _, _, _).
validateComputerUnitMoveDOWNAux(_, 10, _, _).
validateComputerUnitMoveDOWNAux(10, _, _, _).
validateComputerUnitMoveDOWNAux(_, 10, _, _).

validateComputerUnitMoveRIGHTAux(Row, Column, Board, LineBoard) :-
        validateComputerUnitMoveUP(Row, Column, Board, LineBoard);
        validateComputerUnitMoveDOWN(Row, Column, Board, LineBoard);
        validateComputerUnitMoveRIGHT(Row, Column, Board, LineBoard).

validateComputerUnitMoveRIGHT(Row, Column, Board, LineBoard) :-
        TempColumn is Column + 1,
        getCommunication(LineBoard, Row, Column, LinePiece),
        getPiece(Board, Row, TempColumn, Piece),
        Piece == 'empty',
        LinePiece == 'l',
        validateComputerUnitMoveAux(Row, TempColumn),
        setCell(Row, Column, a, Board, NextBoard),
        validateComputerUnitMoveRIGHTAux(Row, TempColumn, NextBoard, LineBoard).

validateComputerUnitMoveLEFTAux(Row, Column, Board, LineBoard) :-
        validateComputerUnitMoveUP(Row, Column, Board, LineBoard);
        validateComputerUnitMoveDOWN(Row, Column, Board, LineBoard);
        validateComputerUnitMoveLEFT(Row, Column, Board, LineBoard).

validateComputerUnitMoveLEFT(Row, Column, Board, LineBoard) :-
        TempColumn is Column - 1,
        getCommunication(LineBoard, Row, Column, LinePiece),
        getPiece(Board, Row, TempColumn, Piece),
        Piece == 'empty',
        LinePiece == 'l',
        validateComputerUnitMoveAux(Row, TempColumn),
        setCell(Row, Column, a, Board, NextBoard),
        validateComputerUnitMoveLEFTAux(Row, TempColumn, NextBoard, LineBoard).

validateComputerUnitMoveDOWNAux(Row, Column, Board, LineBoard) :-
        validateComputerUnitMoveDOWN(Row, Column, Board, LineBoard);
        validateComputerUnitMoveLEFT(Row, Column, Board, LineBoard);
        validateComputerUnitMoveRIGHT(Row, Column, Board, LineBoard).

validateComputerUnitMoveDOWN(Row, Column, Board, LineBoard) :-
        TempRow is Row + 1,
        getCommunication(LineBoard, Row, Column, LinePiece),
        getPiece(Board, TempRow, Column, Piece),
        Piece == 'empty',
        LinePiece == 'l',
        validateComputerUnitMoveAux(TempRow, Column),
        setCell(Row, Column, a, Board, NextBoard),
        validateComputerUnitMoveDOWNAux(TempRow, Column, NextBoard, LineBoard).

validateComputerUnitMoveUPAux(Row, Column, Board, LineBoard) :-
        validateComputerUnitMoveUP(Row, Column, Board, LineBoard);
        validateComputerUnitMoveLEFT(Row, Column, Board, LineBoard);
        validateComputerUnitMoveRIGHT(Row, Column, Board, LineBoard).

validateComputerUnitMoveUP(Row, Column, Board, LineBoard) :-
        TempRow is Row - 1,
        getCommunication(LineBoard, Row, Column, LinePiece),
        getPiece(Board, TempRow, Column, Piece),
        Piece == 'empty',
        LinePiece == 'l',
        validateComputerUnitMoveAux(TempRow, Column),
        setCell(Row, Column, a, Board, NextBoard),
        validateComputerUnitMoveUPAux(TempRow, Column, NextBoard, LineBoard).
        
validateComputerUnitMove(Row, Column, Board, LineBoard) :-
        validateComputerUnitMoveUP(Row, Column, Board, LineBoard);
        validateComputerUnitMoveDOWN(Row, Column, Board, LineBoard);
        validateComputerUnitMoveRIGHT(Row, Column, Board, LineBoard);
        validateComputerUnitMoveLEFT(Row, Column, Board, LineBoard);
        true.

/*finds next position where to draw communication line*/
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

/*sets cell o l*/
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

updateLineBoard(Row, Column, LineBoard, NewLineBoard) :-
        updateLineBoardAux(l, Row, Column, LineBoard, NewLineBoard).


/*clears LineBoard: sets cells to empty*/
cleanLine([], _, _). 

cleanLine([Piece|T], Piece, [empty|NewTail]) :-
        cleanLine(T, Piece, NewTail).

cleanLine([H|T], Piece, [H|NewTail]) :-
        cleanLine(T, Piece, NewTail).

cleanBoard([], _). 

cleanBoard([E1|Es], [H|T]) :-
        cleanLine(E1, l, H),
        cleanBoard(Es, T).

finishMove(Piece, Finish) :-
        node(Piece), Finish == 1 ->
                retract(state(NewBoard, LineBoard)),
                cleanBoard(LineBoard, TempLineBoard),
                findNode(NewBoard, 1, NewRow1, NewColumn1, node1),
                setCell(NewRow1, NewColumn1, l, TempLineBoard, NewTempLineBoard2),
                updateLineBoard(NewRow1, NewColumn1, NewTempLineBoard2, NewTempLineBoard3),
                findNode(NewBoard, 1, NewRow2, NewColumn2, node2),
                setCell(NewRow2, NewColumn2, l, NewTempLineBoard3, NewTempLineBoard),
                updateLineBoard(NewRow2, NewColumn2, NewTempLineBoard, NewLineBoard),
                assert(state(NewBoard, NewLineBoard)),
                assert(nodePosition(NewBoard, NewLineBoard));
        fail.

moveNode(Row, Column, NewRow, NewColumn, Board, LineBoard, Piece, Finish) :-
        validateNodeMove(Row, Column, NewRow, NewColumn, Board) ->
                setCell(Row, Column, empty, Board, NewBoardTemp),
                setCell(NewRow, NewColumn, Piece, NewBoardTemp, NewBoard),
                assert(state(NewBoard, LineBoard)),
                Finish is 1;
        assert(state(Board, LineBoard)),
        Finish is 0.

moveUnit(Row, Column, NewRow, NewColumn, Board, LineBoard, Piece, Finish) :-
        validateUnitMove(Row, Column, NewRow, NewColumn, Board, LineBoard) ->
                setCell(Row, Column, empty, Board, NewBoardTemp),
                setCell(NewRow, NewColumn, Piece, NewBoardTemp, NewBoard),
                assert(state(NewBoard, LineBoard)),
                Finish is 0;
        assert(state(Board, LineBoard)),
        Finish is 0.

move(Piece, Row, Column, NewRow, NewColumn, Board, LineBoard, Piece, Finish) :-
        node(Piece) ->
                moveNode(Row, Column, NewRow, NewColumn, Board, LineBoard, Piece, Finish);
                moveUnit(Row, Column, NewRow, NewColumn, Board, LineBoard, Piece, Finish).
        

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
                move(Piece, Row, Column, NewRow, NewColumn, Board, LineBoard, Piece, Finish),
                finishMove(Piece, Finish),
                !.

finish(Player, Board):-
        nextPlayer(Player, Next),
        nodePlayer(Next, Node),
        findNode(Board, 1, Row, Column, Node),
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
        Type =:= 2 -> chooseLevel;
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

moveRanUnitAux(Row, Column, LengthMoves, Moves, Piece) :-
        LengthMoves \== 0 ->
                retract(state(Board, LineBoard)),
                Length is LengthMoves + 1,
                random(1, Length, Move),
                nth1(Move, Moves, NewPos),
                nth1(1, NewPos, FinalRow),
                nth1(2, NewPos, FinalColumn),
                moveUnit(Row, Column, FinalRow, FinalColumn, Board, LineBoard, Piece, _);
        true.

moveRandUnit([], _).
                                                                    
moveRandUnit([L1|Ls],Piece):-
        nth1(1, L1, Row),
        nth1(2, L1, Column),
        random(0, 4, WillPlay),
        WillPlay > 1 ->
                retract(state(Board, LineBoard)),
                assert(validateComputerList([])),
                validateComputerUnitMove(Row, Column, Board, LineBoard),
                assert(state(Board, LineBoard)),
                retract(validateComputerList(Moves)),
                length(Moves, LengthMoves),
                moveRanUnitAux(Row, Column, LengthMoves, Moves, Piece),
                moveRandUnit(Ls,Piece);
        moveRandUnit(Ls,Piece).

randPlayAux(NodeRow, NodeColumn, Board, LineBoard, NodeMoves, Piece) :-
        length(NodeMoves, LengthNodeMoves),
        LengthNodeMoves \== 0 ->
                Length is LengthNodeMoves + 1,
                random(1, Length, NodeMove),
                nth1(NodeMove, NodeMoves,NewNodePos),
                nth1(1, NewNodePos, FinalNodeRow),
                nth1(2, NewNodePos, FinalNodeColumn),
                moveNode(NodeRow, NodeColumn, FinalNodeRow, FinalNodeColumn, Board, LineBoard, Piece, _),
                assert(nodePosition(FinalNodeRow, FinalNodeColumn));
        assert(nodePosition(NodeRow, NodeColumn)).
        
randPlay(Player):- 
        retract(state(Board, LineBoard)),

        unitPlayer(Player, Piece),
        nodePlayer(Player, Node),
        assert(randlist([])),
        findUnits(Board, 1, 1, Piece),
        retract(randlist(Units)),
        assert(state(Board, LineBoard)),
        moveRandUnit(Units, Piece),
        
        retract(state(NewBoard, NewLineBoard)),
        findNode(NewBoard, 1, NodeRow, NodeColumn, Node),
        findall([NewNodeRow, NewNodeColumn], (((NewNodeRow is NodeRow + 1,
                                              NewNodeColumn is NodeColumn,
                                              validateNodeMove(NodeRow, NodeColumn, NewNodeRow, NewNodeColumn, NewBoard));
                                              (NewNodeRow is NodeRow - 1,
                                              NewNodeColumn is NodeColumn,
                                              validateNodeMove(NodeRow, NodeColumn, NewNodeRow, NewNodeColumn, NewBoard));
                                              (NewNodeRow is NodeRow,
                                              NewNodeColumn is NodeColumn + 1,
                                              validateNodeMove(NodeRow, NodeColumn, NewNodeRow, NewNodeColumn, NewBoard));
                                              (NewNodeRow is NodeRow,
                                              NewNodeColumn is NodeColumn - 1,
                                              validateNodeMove(NodeRow, NodeColumn, NewNodeRow, NewNodeColumn, NewBoard)))), NodeMoves),
        randPlayAux(NodeRow, NodeColumn, NewBoard, NewLineBoard, NodeMoves, Node),
        finishMove(Node, 1),
        retract(state(TempBoard, TempLineBoard)),
        displayBoard(TempBoard, TempLineBoard),
        assert(state(TempBoard, TempLineBoard)).

nextHCMove(Player):-
        Player == p1 ->
        nextMove(Player);
        randPlay(Player),!.
/*
abs(N,Abs):-
        N<0 ->
        Abs is 0 - N;
        Abs is N.

score(R1, C1, R2, C2, Score):-
        Dif1 is R1 - R2,
        Dif2 is C1 - C2,
        abs(Dif1, DifR),
        abd(Dif2, DifC),
        Score is DifR + DifC.

bestPlayAux(Board, [U1|Us], Row, Column, NodeRow, NodeColumn, Score):-
        retract(bestScore(BestScore)),
        nth1(1,U1,TempRow),
        nth1(2,U1,TempColumn),
        bestPiecePlay(TempRow,TempColumn,Row,Column),
        score(TempRow, TempColumn, NodeRow, NodeColumn, TempScore),
        TempScore < BestScore ->
        assert(bestScore(TempScore)),
        bestPieceAux(Board,Us,TempRow,TempColumn,NodeRow, NodeColumn,TempScore);
        bestPieceAux(Board,Us,Row,Column,Score).


bestPlay(Board, Units, Row, NewRow, Column, NewColumn):-
        findNode(Board, 1, NodeRow, NodeColumn, node1),
        bestPlayAux(Board, Units, Row, NewRow, Column, NewColumn, NodeRow, NodeColumn, Score).

hardPlay:-
        assert(randlist([])),
        findUnits(Board, 1, 1, unit1),
        retract(randlist(Units)),
        bestPlay(Board, Units, Row, NewRow, Column, NewColumn).        
        

nextHardMove(Player):-
        Player == p1 ->
        nextMove(Player);
        hardPlay(Player),!.

hard:-
        repeat,
                retract(player(Player)), nl,
                write('Player: '), write(Player), nl, nl,
                nextHardMove(Player),
                retract(nodePosition(Board, _)),
                nextPlayer(Player, Next),
                assert(player(Next)),
                finish(Player, Board),
                !.        
 */       
playHC :- 
        repeat,
                retract(player(Player)), nl,
                write('Player: '), write(Player), nl, nl,
                nextHCMove(Player),
                retract(nodePosition(Board, _)),
                nextPlayer(Player, Next),
                assert(player(Next)),
                finish(Player, Board),
                !.

playCC :- 
        repeat,
                retract(player(Player)), nl,
                write('Player: '), write(Player), nl, nl,
                randPlay(Player),
                retract(nodePosition(Board, _)),
                nextPlayer(Player, Next),
                assert(player(Next)),
                finish(Player, Board),
                !.


match :-
        init(Type),
        play(Type),
        retract(state(_,_)).