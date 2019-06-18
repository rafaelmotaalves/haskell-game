module Entities.Obstacle where
    
    moveSize :: Float
    moveSize = 5

    moveLeft :: (Float, Float) -> (Float, Float)
    moveLeft (x, y) = (x - moveSize , y)