:-use_module(library(lists)).
:-use_module(library(between)).
:-use_module(library(random)).
:-consult(drawings).
:-consult(menu).

% switch_player(+Player, -NewPlayer)
% Switch to the next player
switch_player(1,2).
switch_player(2,1).

% on_board(+Board, +Position) 
% Check if the Position is within the board
on_board(Board,X-Y):-
    length(Board,BoardSize),
    between(1,BoardSize,X),
    between(1,BoardSize,Y).

% move(+GameState,+Move,-NewGameState)
% Move a piece from a stack
% Given a move we check whether the move is valid, taking into account the rules of the game, 
% if it is we move the piece and add a piece to each friendly piece in the sight, switch to the next player playing and return a new game state
move(GameState, [X-Y, used(StartX-StartY)], NewGameState) :-
    [Board, Player, BoardSize, P1, P2, OptRule | Tail] = GameState,
    valid_stack_move([Board, Player, OptRule], [X-Y, used(StartX-StartY)]), 
    move_piece(Board, Player, StartX-StartY, X-Y, TempBoard),
    add_to_friendly_pieces(TempBoard, Player, X, Y, NewBoard,StartX-StartY), 
    switch_player(Player, NewPlayer),
    NewGameState = [NewBoard, NewPlayer, BoardSize, P1, P2, OptRule | Tail].

% Move a new piece to the board
% Similar execution of a stack move, since it is a new piece being placed, for validation we just need to check if the move position is free and inside the board.
move(GameState, [X-Y, new], NewGameState) :-
    [Board, Player | Tail] = GameState,
    on_board(Board,X-Y),
    valid_placement([Board, Player], [X-Y, new]),
    place_piece(Board, Player, X-Y, TempBoard), 
    add_to_friendly_pieces(TempBoard, Player, X, Y, NewBoard,[]), 
    switch_player(Player, NewPlayer),
    NewGameState = [NewBoard,NewPlayer | Tail].

% valid_placement(+Board,+Move)
% Check for a valid piece placement
valid_placement([Board | _], [X-Y, new]) :-
    empty_position(Board, X, Y). 

% empty_position(+Board, +X, +Y)
% Check if position is empty
empty_position(Board, X, Y) :-
    nth1(Y, Board, Row),
    nth1(X, Row, 0).

% place_piece(+Board, +Player, +Position, -NewBoard)
% Place a piece on the board
place_piece(Board, Player, X-Y, NewBoard):-
    nth1(Y,Board,Row),
    replace(X,Player-1,Row,NewRow), 
    replace(Y,NewRow,Board,NewBoard).   

% move_piece(+Board, +Player, +StackPosition, +MovePosition, -NewBoard)
% Remove a piece from stack and place on another position
move_piece(Board, Player, StartX-StartY, X-Y, NewBoard):-
    nth1(StartY,Board,Row),
    nth1(StartX,Row,Player-Count),
    NewCount is Count-1,
    replace(StartX,Player-NewCount,Row,NewRow), 
    replace(StartY,NewRow,Board,MidBoard),
    place_piece(MidBoard,Player,X-Y,NewBoard).

% replace(+Index, +Element, +List, -Result)
% Replace an element on a list
replace(Index, Element, List, Result) :-
    nth1(Index, List, _, Rest),
    nth1(Index, Result, Element, Rest).

% add_to_friendly_pieces(+Board, +Player, +X, +Y, -NewBoard, +Moved)
% Add pieces to friendly pieces in line of sight
add_to_friendly_pieces(Board, Player, X, Y, NewBoard, Moved) :-
    findall([PX-PY], (in_line_of_sight(Board, X-Y, PX-PY, Player), [PX-PY] \= [Moved]), FriendlyPieces),
    add_pieces_to_positions(Board, FriendlyPieces, Player, NewBoard).

% in_line_of_sight(+Board, +PiecePosition, +OtherPosition, Player)
% Check if a piece is in line of sight
in_line_of_sight(Board, X-Y, PX-PY, Player):-
    check_orthogonal(Board, X-Y, PX-PY, Player);
    check_diagonals(Board, X-Y, PX-PY, Player).

% check_orthogonal(+Board, +PiecePosition, +OtherPosition, +Player)
% Check for piece in orthogonal directions
check_orthogonal(Board, X-Y, PX-PY, Player) :-
    check_direction(Board, X-Y, 0, 1, PX-PY, Player). % Up
check_orthogonal(Board, X-Y, PX-PY, Player) :-
    check_direction(Board, X-Y, 0, -1, PX-PY, Player). % Down
check_orthogonal(Board, X-Y, PX-PY, Player) :-
    check_direction(Board, X-Y, 1, 0, PX-PY, Player). % Right
check_orthogonal(Board, X-Y, PX-PY, Player) :-
    check_direction(Board, X-Y, -1, 0, PX-PY, Player). % Left
    
% check_diagonals(+Board, +PiecePosition, +OtherPosition, +Player)
% Check for piece in diagonal directions
check_diagonals(Board, X-Y, PX-PY, Player) :-
    opposite_parity(X-Y),
    check_direction(Board, X-Y, 1, 1, PX-PY, Player). % Diagonal Up-Right
check_diagonals(Board, X-Y, PX-PY, Player) :-
    opposite_parity(X-Y),
    check_direction(Board, X-Y, -1, -1, PX-PY, Player). % Diagonal Down-Left
check_diagonals(Board, X-Y, PX-PY, Player) :-
    opposite_parity(X-Y),
    check_direction(Board, X-Y, 1, -1, PX-PY, Player). % Diagonal Down-Right
check_diagonals(Board, X-Y, PX-PY, Player) :-
    opposite_parity(X-Y),
    check_direction(Board, X-Y, -1, 1, PX-PY, Player). % Diagonal Up-Left

% opposite_parity(+Position)
% Check if the coordinates of the piece position has opposite parity
opposite_parity(X-Y):-
    (X mod 2 =\= 0, Y mod 2 =\= 0);
    (X mod 2 =:= 0, Y mod 2 =:= 0).

% check_direction(+Board, +Position, +DX, +DY, +Player)
% Check if piece is in the current direction
check_direction(Board, X-Y, DX, DY, PX-PY, Player):-
    NextX is X + DX,
    NextY is Y + DY,
    on_board(Board,NextX-NextY),
    nth1(NextY,Board,Row),
    nth1(NextX,Row,Piece),
    check_piece(Piece, Player, NextX, NextY, Board, DX, DY, PX-PY).

% check_piece(+Piece, +Player, +NextX, +NextY, +Board, +DX, +DY, +PieceInSightPosition)
% Check if the current position, there is a friendly piece, enemy piece or is empty
check_piece(Player-_, Player, NextX, NextY, _, _, _, NextX-NextY).
check_piece(0, Player, NextX, NextY, Board, DX, DY, PX-PY):-
    check_direction(Board,NextX-NextY, DX, DY, PX-PY,Player).
check_piece(_, _, _, _, _, _, _, _):- false.

% add_piece_to_positions(+Board, +Positions, +Player, -NewBoard)
% Adds a piece to all the positions received
add_pieces_to_positions(NewBoard, [], _, NewBoard).  
add_pieces_to_positions(Board, [[PX-PY] | Rest], Player, NewBoard) :-
    add_piece_to_position(Board, PX-PY, Player, TempBoard),  
    add_pieces_to_positions(TempBoard, Rest, Player, NewBoard). 

% add_piece_to_position(+Board, +Position, +Player, -NewBoard)
% Adds a piece to the received position
add_piece_to_position(Board, X-Y, Player, NewBoard) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Player-Count),
    NewCount is Count + 1, 
    replace(X, Player-NewCount, Row, NewRow), 
    replace(Y, NewRow, Board, NewBoard).

% empty_adjacent_positions(+Board, +Position, -EmptyPositions)
% Gets the adjacent empty positions to a given position of a piece
empty_adjacent_positions(Board, X-Y, EmptyPositions):-
    check_adjacent_orthogonal(Board, X-Y, OrthogonalPositions),
    check_adjacent_diagonal(Board, X-Y, DiagonalPositions),
    append(OrthogonalPositions,DiagonalPositions,EmptyPositions).

% check_adjacent_orthogonal(+Board, +Position, -OrthogonalPositions)
% Gets the adjacent orthogonal empty positions
check_adjacent_orthogonal(Board, X-Y, OrthogonalPositions):-
    AdjDeltas = [(-1, 0), (1, 0), (0, -1), (0, 1)],
    findall(
        PX-PY,
        (member((DX, DY), AdjDeltas),
        check_adjacent(Board, X-Y, DX, DY, PX-PY)),
        OrthogonalPositions
        ).

% check_adjacent_diagonal(+Board, +Position, -DiagonalPositions)
% Gets the adjacent diagonal empty positions
check_adjacent_diagonal(Board, X-Y, DiagonalPositions):-
    opposite_parity(X-Y),
    AdjDeltasDiagonal = [(-1, -1), (1, 1), (1, -1), (-1, 1)],
    findall(
        PX-PY,
        (member((DX, DY), AdjDeltasDiagonal),
        check_adjacent(Board, X-Y, DX, DY, PX-PY)),
        DiagonalPositions
        ).

check_adjacent_diagonal(_,_, []).

% check_adjacent(+Board, +Position, +DX, +DY, -AdjacentPosition) 
% Gets the position of an empty adjacent position
check_adjacent(Board, X-Y, DX, DY, PX-PY):-
    NextX is X + DX,
    NextY is Y + DY,
    on_board(Board,NextX-NextY),
    empty_position(Board, NextX, NextY),
    PX = NextX,
    PY = NextY.

% are_adjacent(+Position1, +Position2)
% Check if the given positions are adjacent
are_adjacent(X1-Y1, X2-Y2) :-
    (   X1 =:= X2, (Y1 is Y2 + 1; Y1 is Y2 - 1);   
        Y1 =:= Y2, (X1 is X2 + 1; X1 is X2 - 1);
     (X1 is X2 + 1; X1 is X2 - 1),(Y1 is Y2 + 1; Y1 is Y2 - 1) 
    ).

% has_stack(+Board, +Player, +X, +Y, +Count)
% Check if for a given position the piece is a stack
has_stack(Board, Player, X, Y, Count) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Player-Count),
    Count > 1.   

% existing_stacks(+Board, +Player, -CountStacks)
% Get a list with the number of pieces from existing stacks, from highest to lowest
existing_stacks(Board, Player, CountStacks) :-
    findall(Count, (has_stack(Board,Player, _, _, Count)), Stacks),
    sort(Stacks, SortedStacks),    
    reverse(SortedStacks, CountStacks).

% check_empty_adjacent_positions(+Board, +Positions , -EmptyPositions)
% Gets the adjacent empty positions from the highest stacks
check_empty_adjacent_positions(Board, [Pos | RestPositions], EmptyPositions) :-
    empty_adjacent_positions(Board, Pos, AdjPositions), 
    check_empty_adjacent_positions(Board, RestPositions, RestEmptyPositions), 
    append(AdjPositions, RestEmptyPositions, MidEmptyPositions),
    remove_dups(MidEmptyPositions, EmptyPositions). 

check_empty_adjacent_positions(_, [], []).

% highest_stacks(+Board, +Player, +Highest, -Positions)
% Get the positions of the stacks with highest number of pieces
highest_stacks(Board, Player, Highest, Positions) :-
    findall(X-Y, (has_stack(Board, Player, X, Y, Highest)), Positions).

% find_valid_stack_moves(+Board, +Player, +Stacks, -Positions,-EmptyPositions)
% Get the valid stack moves
find_valid_stack_moves(Board,Player,[ Highest | _], Positions, EmptyPositions):-
    highest_stacks(Board, Player, Highest, Positions), 
    check_empty_adjacent_positions(Board, Positions, EmptyPositions),
    EmptyPositions \= [].

find_valid_stack_moves(Board, Player, [_ | NextHighest], Positions, EmptyPositions) :-
    find_valid_stack_moves(Board, Player, NextHighest, Positions, EmptyPositions).

% valid_stack_move(+Board_Player_OptRule, +Move)
% Check if the given move from stack is valid
valid_stack_move([Board, Player, 0], [X-Y, used(StartX-StartY)]) :-
    existing_stacks(Board, Player, Stacks),
    find_valid_stack_moves(Board,Player, Stacks,Positions,EmptyPositions), !,
    are_adjacent(StartX-StartY, X-Y),
    member(X-Y,EmptyPositions),
    member(StartX-StartY,Positions).
valid_stack_move([Board, Player, 1], [X-Y, used(StartX-StartY)]) :-
    existing_stacks(Board, Player, Stacks),
    find_valid_stack_moves(Board,Player, Stacks,Positions,EmptyPositions), !,
    member(X-Y,EmptyPositions),
    member(StartX-StartY,Positions).

% valid_moves(+GameState,-Moves)
% Gets the valid moves for the current player
% First we check if the current player has stacks, if it has we get all the valid stack moves, otherwise we get the valid position where we can place a piece
valid_moves(GameState, Moves) :-
    [Board,Player | _] = GameState,
    existing_stacks(Board,Player,Stacks),
    (   Stacks \= [] -> findall(Move, valid_used_move(GameState, Move), Moves);
        findall(Move, valid_new_move(GameState, Move), Moves)
    ).

% valid_used_move(+GameState, +Move)
% Get the valid stack moves
valid_used_move(GameState, [X-Y, used(StartX-StartY)]) :-
    [Board, Player, BoardSize, _, _, OptRule | _] = GameState,
    between(1, BoardSize, X),
    between(1, BoardSize, Y),
    between(1, BoardSize, StartX),
    between(1, BoardSize, StartY),
    valid_stack_move([Board, Player, OptRule], [X-Y, used(StartX-StartY)]).

% valid_new_move(+GameState, +Move)
% Get the new valid moves
valid_new_move(GameState, [X-Y, new]) :-
    [Board, Player | _] = GameState,
    on_board(Board, X-Y),
    valid_placement([Board, Player], [X-Y, new]).
    
% game_over(+GameState, -State)
% Check if the game is over, if it is return the winner name
% If no moves are returned from the valid_moves function, the game is over. So the next player is the winner 
game_over(GameState, Winner):-
    [_, Player, _, _, _, _, Player1Name, Player2Name | _] = GameState,
    valid_moves(GameState, Moves),
    length(Moves,0),
    switch_player(Player,NextPlayer),
    selectWinner([Player1Name,Player2Name],NextPlayer,Winner).

% selectWinner(+BothPlayerNames, +NextPlayer, -WinnerName)
% Get the winner name
selectWinner([Player1Name,_],1,Player1Name).
selectWinner([_,Player2Name],2,Player2Name).

% congratulate(+Winner)
% Print the game over screen congratulating the winner
congratulate(Winner) :-
	nl, nl, nl,
	format('~tGAME OVER~t~74|~n~n~tCongratulations ~s! ~t~75|~n~n~tYou are the winner!~t~74|\n\n', [Winner]).



/*
In level 1 the computer randomly chooses a move from all the valid ones
*/
choose_move(1, _GameState, Moves, Move) :- 
	random_select(Move, Moves, _Rest).
/*
In level 2 the computer chooses the move that gives him the best value immediately
*/
choose_move(2, GameState, Moves, Move) :- 
	[_, Player | _] = GameState,
	setof(Value-Move, NewState^( 
		member(Move, Moves),
		move(GameState, Move, NewState),
		value(NewState, Player, Value) ),
	[_V-Move|_]).
/*
In level 3 the computer uses a minimax algorithm to choose the best move while seeing a few moves ahead. The computer assumes that the adversary will play optimally all the time
(according to the value/3 predicate) and chooses the move that will give him the worst possible game state, that is, chooses the the move that gives him the worst move out of all the best ones. This is done recursively and using th minmax/6 and minmax_aux/7 predicates.
*/
choose_move(3, GameState, Moves, Move) :-
	[_, Player|_] = GameState,
	switch_player(Player, Adversary),
	minmax(GameState, Adversary, Moves, -10001, [], Move).

minmax(_GameState, _, [], _MaxValue, MaxMove, MaxMove).
minmax(GameState, Adversary, [NextMove | T], MaxValue, MaxMove, Move) :-
	move(GameState, NextMove, NextState),
	valid_moves(NextState, NextMoves),
	minmax_aux(NextState, NextMoves, Adversary, 10001, Min, 1, min),
	update_max(MaxValue-MaxMove, Min-NextMove, NewMaxValue-NewMaxMove),
	minmax(GameState, Adversary, T, NewMaxValue, NewMaxMove, Move).

% update value and move if the new value is greater than the previous
update_max(PrevVal-_PrevMove, NewVal-NewMove, NewVal-NewMove) :-
	NewVal > PrevVal, !.
% new value is not greater than previous so the maximum value move stays the same
update_max(PrevVal-PrevMove, _NewVal-_NewMove, PrevVal-PrevMove).
	

% stop searching for more moves and evaluate current board
minmax_aux(State, _, Adversary, _, Value, 0, _) :-
	value(State, Adversary, Value), !.

% no more moves available so stops and returns Value
minmax_aux(State, [], Adversary, 10001, Value, _, min) :-
	value(State, Adversary, Value), !.

% no more moves available so stops and returns Value
minmax_aux(State, [], Adversary, -10001, Value, _, max) :-
	value(State, Adversary, Value), !.

% all valid moves checked, so returns the maximum or minimum value
minmax_aux(_, [], _, Value, Value, _, _).
minmax_aux(State, [NextMove | T], Adversary, Min, FinalMin, Depth, min) :-
	move(State, NextMove, NextState),
	valid_moves(NextState, NextMoves),
	NewDepth is Depth - 1,
	minmax_aux(NextState, NextMoves, Adversary, -10001, NewMin, NewDepth, max),
	UpdatedMin is min(Min, NewMin),
	minmax_aux(State, T, Adversary, UpdatedMin, FinalMin, Depth, min).

minmax_aux(State, [NextMove | T], Adversary, Max, FinalMax, Depth, max) :-
	move(State, NextMove, NextState),
	valid_moves(NextState, NextMoves),
	NewDepth is Depth - 1,
	trace,
	minmax_aux(NextState, NextMoves, Adversary, 10001, NewMax, NewDepth, min),
	UpdatedMax is max(Max, NewMax),
	minmax_aux(State, T, Adversary, UpdatedMax, FinalMax, Depth, max).


/*
The value is of a game state in the Player's point of view is calculated using the number of valid moves each player has.
The lower the value the better for the Player. 
The calculation is made so that winning has the best value and losing has the worst value. We also emphasize the adversary having less valid moves than the other way around.
*/
value([Board, CurPlayer | Rest], Player, Value) :-
	valid_moves([Board, CurPlayer | Rest], Moves1),
	switch_player(CurPlayer, NextPlayer),
	valid_moves([Board, NextPlayer | Rest], Moves2),
	length(Moves1, N1),
	length(Moves2, N2),
	evaluate(N1, N2, CurPlayer, Player, Value).

% value of the board for the player that is currently playing
evaluate(N1, N2, PlayerEvaluating, PlayerEvaluating, Value) :-
	get_value(N2, N1, Value), !.
% value of the board for the player that will play next or played before
evaluate(N1, N2, _, _, Value) :-
	get_value(N1, N2, Value).

% calculates value of the board
get_value(0, _, -10000) :- !.
get_value(_, 0, 10000) :- !.
get_value(N1, N2, Value) :-
	Value is N1 - log(2, N2).

/*
This predicate is used for both human and computer move choice. In case of human the choose_move_io/3 predicate is used to take care of asking the user what move he wants to play
and also of the input of the move itself. It first checks if he has any stacks to know which type of move he can make and ask accordingly. 
In case of a computer we call the choose_move/4 predicate that deals with this choice differently depending of the level (the strategy used in each is commented there).
*/
choose_move(GameState, 0-Level, Move):-
	valid_moves(GameState, ValidMoves),
	choose_move(Level, GameState, ValidMoves, Move).
choose_move([Board, Player, BoardSize, _P1Type, _P2Type, _OptRule, P1Name, P2Name], _, Move) :-
	nth1(Player, [P1Name, P2Name], Name),
	existing_stacks(Board, Player, Stacks),
	length(Stacks, N),
	format('\n\nIt\'s ~s\'s turn\n', [Name]),
	choose_move_io(BoardSize, N, Move).

choose_move_io(BoardSize, 0, [X-Y, new]) :-
	write('Choose where to put the next piece:\n\n'),
	repeat,
	write('Column: '),
	read_number(X),
	between(1, BoardSize, X), !,
	repeat,
	write('Line: '),
	read_number(Y),
	between(1, BoardSize, Y), !.
choose_move_io(BoardSize, _, [X-Y, used(Xused-Yused)]) :-
	write('Choose the stack to remove a piece:\n'),
	repeat,
	write('Column: '),
	read_number(Xused),
	between(1, BoardSize, Xused), !,
	repeat,
	write('Line: '),
	read_number(Yused),
	between(1, BoardSize, Yused), !,
	write('Choose where to put that piece:\n'),
	repeat,
	write('Column: '),
	read_number(X),
	between(1, BoardSize, X), !,
	repeat,
	write('Line: '),
	read_number(Y),
	between(1, BoardSize, Y), !.
