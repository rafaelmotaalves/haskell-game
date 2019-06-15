module Entities.Game (restartGame) where

data Game = Game { player:: (Int, Int), obstacles::[(Int, Int)] }

defaultPlayerPos = (5, 5)

restartGame :: Game -> Game
restartGame game = game { player = defaultPlayerPos, obstacles = [] }

