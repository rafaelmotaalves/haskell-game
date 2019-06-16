
module Entities.Game where

    data Game = Game { player:: (Float, Float), obstacles::[(Float, Float)] }

    defaultPlayerPos = (0, 20)

    restartGame :: Game
    restartGame = Game { player = defaultPlayerPos, obstacles = [(150, 15), (-100, 15)] }