
module Entities.Game where
    import Control.Concurrent

    data Game = Game { player:: (Float, Float), obstacles::[(Float, Float)], inJump :: Bool }

    defaultPlayerPos = (0, 20)

    restartGame :: Game
    restartGame = Game { player = defaultPlayerPos, obstacles = [(400, 15), (600, 15)], inJump = False}