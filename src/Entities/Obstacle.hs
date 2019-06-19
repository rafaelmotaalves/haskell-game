module Entities.Obstacle where
    import Control.Concurrent
    import Entities.Draw
    import Entities.Types

    newObstacles :: IO (Obstacles)
    newObstacles = newMVar []

    moveAllObstacles :: [(Float, Float)] -> Float -> [(Float, Float)]
    moveAllObstacles o d = (filter atScreen $ map (moveLeft d) o)

    moveSize :: Float
    moveSize = 5 

    moveLeft :: Float -> (Float, Float) -> (Float, Float)
    moveLeft m (x, y) = (x - (moveSize * m) , y)

    atScreen :: (Float, Float) -> Bool
    atScreen (x, y) = round x > - (width `div` 2)