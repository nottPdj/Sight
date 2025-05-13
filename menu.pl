:- use_module(library(between)).
:- use_module(library(lists)).

% generates the initial state from the user configuration
initial_state([BoardSize, Player1, Player2, Player1Name, Player2Name, OptRule], [Board, 1, BoardSize, Player1, Player2, OptRule, Player1Name, Player2Name]) :-
	length(Row, BoardSize),
    maplist(=(0), Row),
	length(Board, BoardSize),
    maplist(=(Row), Board).

% menu to get the game configuration
get_configuration([BoardSize, Player1, Player2, Player1Name, Player2Name, OptRule]) :-
	write('\33\[2J'),
	draw_header,
	get_players(Player1, Player2, Player1Name, Player2Name), 
	draw_separator,
	get_board_size(BoardSize), nl,
	draw_separator,
	optional_rule(OptRule), nl.

% draws the game header
draw_header :-
	write('***********************\n'),
	write('*                     *\n'),
	write('*        SIGHT        *\n'),
	write('*                     *\n'),
	write('*      Game Menu      *\n'),
	write('*                     *\n'),
	write('***********************\n\n').

draw_separator :- write('-----------------------\n\n').

% get the players type, name and difficulty in case of computer
get_players(Player1, Player2, Player1Name, Player2Name) :-
	write('Choose players (Player 1 is the first to play):\n\n'),
	write('- PLAYER 1\n'),
	get_player(Player1, Player1Name),
	write('- PLAYER 2\n'),
	get_player(Player2, Player2Name).

% ask the user the type of the player and the difficulty in case of computer
get_player(Player, Name) :-
	repeat,
	write('Name (3 characters min): '),
	read_string(Name),
	length(Name, L),
	L >= 3, !,
	write('\n0 - CPU    1 - Human\n'),
	repeat,
	write('Type the number of selected choice: '),
	read_number(PlayerType),
	between(0, 1, PlayerType), !,
	nl, choose_level(PlayerType, Player), nl.
	
% player is human (1) so no level is needed
choose_level(1, 1).
% get the level if player is of type computer (0)
choose_level(0, 0-Level) :-
	repeat,
	write('Type the number of the CPU level (1, 2, 3): '),
	read_number(Level), 
	between(1, 3, Level), !.

% get the board size
get_board_size(Size) :-
	repeat,
	write('Choose a board size (odd number between 3 and 9): '),
	read_number(Size),
	between(3, 9, Size),
	Size mod 2 =:= 1, !.

% aks the user if he wants to play with the optional rule (n -0, y - 1)
optional_rule(Option) :-
	repeat,
	write('Do you want to play with the rule -\nPieces removed from stacks can be placed on adjacent positions of any owned stacks (n/y): '),
	get_code(C),
	member(C, "ny"),
	!, skip_line,
	nth0(Option, "ny", C).

% read number from input
read_number(X) :-
	read_char_aux(0, X).

read_char_aux(Acc, Acc) :- peek_code(10), skip_line, !.
read_char_aux(Acc, X) :- 
	get_code(C),
	char_code('0', Zero),
	N is C - Zero,
	between(0, 9, N),
	Acc1 is (Acc*10)+N,
	read_char_aux(Acc1, X).	

% read string from input
read_string(X) :-
    get_line_chars(X).

get_line_chars([]) :- 
    peek_code(10),
    skip_line, !.
get_line_chars([C|X]) :-
    get_code(C),
    get_line_chars(X).