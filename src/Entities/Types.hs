module Entities.Types where
    import Control.Concurrent
    import Graphics.Gloss
    
    data Game = Game { player:: (Float, Float), inJump :: Bool, completedJump :: Bool }
    
    type Difficulty = MVar Float
    type Score = MVar Int
    type Obstacles = MVar [(Float, Float)]
    type GameOver = MVar Bool
    type ObstaclePic = Picture
    type PlayerPic = Picture
    type PlayerPics = [Picture]
    type ObstaclePics = [Picture]
    type PlayerPicFrame = Int
    type ObstaclePicFrame = Int

    type State = (Game, Score, Obstacles, Difficulty, GameOver, ObstaclePic, PlayerPic, PlayerPics, ObstaclePics, PlayerPicFrame, ObstaclePicFrame)
