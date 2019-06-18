
module Entities.Draw (render, background, window) where
    import Control.Concurrent
    import Graphics.Gloss
    
    import Entities.Game
    import Entities.Score

    width, height, offset :: Int
    width = 500
    height = 500
    offset = 0

    window:: Display
    window = InWindow "Runner" (width, height) (offset, offset)

    background :: Color
    background = black

    render :: (Game, Score) -> IO(Picture)
    render (g, s) = do
        score <- readMVar s
        return (pictures ([ renderFloor , renderScore score, renderPlayer (player g)] ++  map (renderObstacle) (obstacles g)))

    renderPlayer :: (Float, Float) -> Picture
    renderPlayer (x, y) = translate (x) (y) $ color red $ circleSolid 20

    renderObstacle :: (Float, Float) -> Picture
    renderObstacle (x, y) = translate (x) (y) $ color blue $ rectangleSolid 10 30

    renderScore :: Int -> Picture
    renderScore score = translate (175) (175) $ scale (0.2) (0.2) $ (Text (show score))
    
    renderFloor :: Picture
    renderFloor = (color green $ (Line [(-500, 0), (500, 0)]))
