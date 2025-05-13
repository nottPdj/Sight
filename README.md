# SIGHT

## Group Sight_2
202206312 - David dos Santos Carvalho Ranito

202204962 - Pedro Daniel Freitas João

202208938 - Tiago Freitas Torres

### Contribution

## Instalation and Execution

## Description of the game
Sight is a game played on the Alquerque board and uses the "line of sight" mechanism. The players can use two type of actions depending on the board, with the objective of leaving the other player with no legal moves.

### Setup
The board used is the Alquerque board with a size chosen by the players. The size is the number of intersections per column and per line and it's either 3, 5, 7 or 9. The board starts empty. The pieces are represented as 'O' for Player 1 and as 'X' for Player 2.

#### Definitions
In this game, a "stack" is two or more pieces stacked together in the same intersection. A stack is represented as an exponent next to the piece, with the number of pieces stacked. A singleton piece is not a stack.

Two pieces on the board are on the "line of sight" if there is a grid line on the board that connects them with no other piece between them. Adjacent pieces are also in line of sight.

### Gameplay
Player 1 is the first to play and after that alternate turns. All the pieces are placed in grid line intersections. On your turn you can perform one of the following actions, depending on the condition of the board:

A) You don't have stacks on the board

If you don't have stacks, then you place a piece on an empty intersection. For all the pieces in the line of sight of the new piece, add another piece to them, making them two-height stacks.

B) You have at least one stack on the board

From one of your stacks, move the top piece to an adjacent empty intersection. If there are multiple stacks, the piece moved is from one of the highest stacks. If all the stacks with that height don't have adjacent empty intersections, then the player can choose from the next highest stacks and so on.

If you have pieces or stacks in the line of sight of the moved piece, add pieces to all them to add height. The piece or stack from which the move originated is unaffected.

### Game End
A player who has no legal moves in his turn loses the game. That is, if the player has stacks and is unable to move a piece from one of his stacks, loses. And if the player has no stacks but there isn't an empty intersection on the board to place a new piece, loses.
