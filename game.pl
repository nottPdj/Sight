:-use_module(library(system)).
:-consult(move).
:-consult(drawings).
:-consult(menu).


/*
gets the inital configuration, transforms it into the initial state, displays the game board and starts the game_cycle
*/
play :-
	get_configuration(GameConfig),
	initial_state(GameConfig, GameState),
	display_game(GameState),
	game_cycle(GameState).

game_cycle(GameState):-
	game_over(GameState, Winner), !,
	congratulate(Winner).
game_cycle(GameState):-
	[_, Player, _, P1Type, P2Type | _Rest] = GameState,
	nth1(Player, [P1Type, P2Type], PlayerType),
	repeat,
	choose_move(GameState, PlayerType, Move),
	move(GameState, Move, NewGameState), !,
	display_game(NewGameState), !,
	sleep(3),
	game_cycle(NewGameState).
