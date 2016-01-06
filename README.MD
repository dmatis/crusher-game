# Crusher Game

A Peg-based Board Game that utilizes a minmax search algorithm

## Start Here

1. Begin by running the crusher haskell file (ie. "ghci crusher.hs" in the terminal window)

## To Play

1. Type 'play history player depth num' where:
  * history : a list of boards of previous game states
  * player : (W) White or (B) Black
  * depth : How deep the minmax algorithm should search for optimal game states
  * n : dimension size of the board

ex. 'play history3W 'W' 3 3'
Will execute a game in the terminal window

2. The crusher program can tell you the ideal move to make for a given game state

ex. 'crusher history3W W 3 3'
Will append the optimal next board state onto the existing history of previous game states