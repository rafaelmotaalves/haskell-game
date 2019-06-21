module Entities.Types where
    import Control.Concurrent

    data Game = Game { player:: (Float, Float), inJump :: Bool, completedJump :: Bool }
    
    type Dificulty = MVar Float
    type Score = MVar Int
    type Obstacles = MVar [(Float, Float)]
    type GameOver = MVar Bool


    type State = (Game, Score, Obstacles, Dificulty, GameOver)
