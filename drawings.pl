:-use_module(library(lists)).

/*
The game board and column and line indexes are dynamic meaning that the board could have any size and still be correctly drawn.
This is because we print the lines and the columns until the respective lists are empty.
To display the game we first reverse the board because line 1 is on the bottom.
The game is displayed line by line, the line with exponents is drawn using display_exponents/1, than in the beggining of a line
with the player pieces a column index is written followed by the display_pieces/3 that displays the pieces and the horizontal lines between them.
The only thing missing before another line of the gameboard is the lines in the board, both vertical and diagonal. This takes 3 lines of output in the terminal
and is done with the display_connections/4 predicate. The vertical lines are always in the same place but the diagonals differ from column to column and line to line.
Both postitions are calculated dynamically.
In the end the column indexes are printed.
*/

display_game([GameBoard, _P, BoardSize | _]) :- 
	write('\33\[2J'), nl, nl, 		% clear screen
	reverse(GameBoard, GameBoardRev),
	display_lines(GameBoardRev, BoardSize, BoardSize).

display_lines( [H|[]], BoardSize, LineCount) :-
	write('     '),
	display_exponents(H),
	format(' ~d   ', [LineCount]),
	display_pieces(H),
	write('\n\n'),
	print_col_numbers(1, BoardSize),
	!.

display_lines( [H|T], BoardSize, LineCount) :-
	write('     '),
	display_exponents(H),
	format(' ~d   ', [LineCount]),
	display_pieces(H),
	write('\n     '),
	display_connections(1, BoardSize, BoardSize, LineCount),
	NewLineCount is LineCount - 1,
	display_lines(T, BoardSize, NewLineCount).

% last exponent (represents number of stacked pieces) of the line
display_exponents( [_P-Exp|[]] ) :- !, format('  ~d\n', [Exp]).
display_exponents( [_|[]] ) :- !, write('   \n').

% display exponents if there is a stack otherwise print a space
display_exponents( [_P-Exp|T] ) :- !, format('  ~d     ', [Exp]), display_exponents(T).
display_exponents( [_|T] ) :- write('        '), display_exponents(T).

% get the character used for representing a piece of a specific player
get_player_piece(1, 'O').
get_player_piece(2, 'X').

% display last piece of the line
display_pieces( [Player-_|[]] ) :- get_player_piece(Player, Piece), format(' ~a \n', [Piece]), !.
display_pieces( [_|[]] ) :- write('   \n'), !.

% display pieces, if there isn't a piece it stays empty
display_pieces( [Player-_|T] ) :- 
	get_player_piece(Player, Piece), 
	format(' ~a -----', [Piece]),
	!,
	display_pieces(T).
display_pieces( [_|T] ) :- 
	write('   -----'),
	display_pieces(T).


% all three lines already displayed
display_connections(3, 1, _, _) :-	
	write(' | \n'), !.

% print end of line and start new one
display_connections(LinesNum, 1, Columns, DiagonalDir) :-
	write(' | \n     '),
	LinesNum1 is LinesNum + 1,
	display_connections(LinesNum1, Columns, Columns, DiagonalDir), !.

% drawing lines of sight in the board, two columns at a time
display_connections(LinesNum, ColumnsLeft, Columns, DiagonalDir) :-
	Sides is LinesNum + LinesNum - 1,
	Mid is 6 - Sides,
	print_two_col(Sides, Mid, DiagonalDir),
	ColumnsLeft2 is ColumnsLeft - 2,
	display_connections(LinesNum, ColumnsLeft2, Columns, DiagonalDir).


% switch between diagonals direction
print_two_col(Sides, Mid, DiagonalDir) :- DiagonalDir mod 2 =:= 1, !, print_two_col(Sides, Mid, '\\', '/').
print_two_col(Sides, Mid, DiagonalDir) :- DiagonalDir mod 2 =:= 0, !, print_two_col(Mid, Sides, '/', '\\').

% prints two columns of a line with diagonals
print_two_col(Sides, Mid, FstChar, SndChar) :-
	Right is Sides - 1,
	write(' |'),
	print_n(Sides, ' '), put_char(FstChar), print_n(Mid, ' '),
	put_char('|'),
	print_n(Mid, ' '), put_char(SndChar), print_n(Right, ' ').

print_col_numbers(BoardSize, BoardSize) :-
	format('      ~d \n', [BoardSize]), !.
print_col_numbers(N, BoardSize) :-
	format('      ~d ', [N]),
	N1 is N + 1,
	print_col_numbers(N1, BoardSize).


% prints S character N times
print_n(0, _S) :- !.
print_n(N, S) :-
	N1 is N-1,
	put_char(S),
	print_n(N1, S).


