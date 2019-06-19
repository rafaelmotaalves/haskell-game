
module Entities.Game where
    import Control.Concurrent
    import Entities.Spawner
    import Entities.Score

    type State = (Game, Score, Obstacles, Dificulty)

    type Dificulty = MVar Float

    data Game = Game { player:: (Float, Float), inJump :: Bool, completedJump :: Bool }

    defaultPlayerPos :: (Float, Float)
    defaultPlayerPos = (0, 0)

    restartGame :: Game
    restartGame = Game { player = defaultPlayerPos, inJump = False, completedJump = True }