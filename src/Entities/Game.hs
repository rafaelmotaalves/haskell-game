
module Entities.Game where
    import Control.Concurrent

    data Game = Game { player:: (Float, Float), obstacles::[(Float, Float)], inJump :: Bool, completedJump :: Bool }

    defaultPlayerPos :: (Float, Float)
    defaultPlayerPos = (0, 0)

    restartGame :: Game
    restartGame = Game { player = defaultPlayerPos, obstacles = [], inJump = False, completedJump = True }