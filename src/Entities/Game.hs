
module Entities.Game where
    import Control.Concurrent

    import Entities.Types

    defaultPlayerPos :: (Float, Float)
    defaultPlayerPos = (0, 0)

    restartGame :: Game
    restartGame = Game { player = defaultPlayerPos, inJump = False, completedJump = True }