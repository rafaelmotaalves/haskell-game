
module Entities.Draw (render, background, window) where
    import Entities.Game
    import Graphics.Gloss
    
    width, height, offset :: Int
    width = 500
    height = 500
    offset = 0

    window:: Display
    window = InWindow "Runner" (width, height) (offset, offset)

    background :: Color
    background = black

    render :: Game -> Picture
    render g = pictures ([(color green $ (Line [(-500, 0), (500, 0)])), renderPlayer (player g)] ++  map (renderObstacle) (obstacles g))

    renderPlayer :: (Float, Float) -> Picture
    renderPlayer (x, y) = translate (x) (y) $ color red $ rectangleSolid 20 40

    renderObstacle :: (Float, Float) -> Picture
    renderObstacle (x, y) = translate (x) (y) $ color blue $ rectangleSolid 10 30

