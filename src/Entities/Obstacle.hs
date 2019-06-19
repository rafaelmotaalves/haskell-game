module Entities.Obstacle where
    
    import Control.Concurrent.STM.TVar
    import Entities.Draw

    moveSize :: Float
    moveSize = 5 

    moveLeft :: Float -> (Float, Float) -> (Float, Float)
    moveLeft m (x, y) = (x - (moveSize * m) , y)

    atScreen :: (Float, Float) -> Bool
    atScreen (x, y) = round x > -(width `div` 2)