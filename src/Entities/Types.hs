module Entities.Types where
    import Control.Concurrent
    import Graphics.Gloss
    
    data Game = Game { player:: (Float, Float), inJump :: Bool, completedJump :: Bool }
    
    type Dificulty = MVar Float
    type Score = MVar Int
    type Obstacles = MVar [(Float, Float)]
    type GameOver = MVar Bool
    type ObstaclePic = Picture
    type PlayerPic = Picture

    type State = (Game, Score, Obstacles, Dificulty, GameOver, ObstaclePic, PlayerPic)
