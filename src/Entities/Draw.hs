
module Entities.Draw (render, background, window, width) where
    import Graphics.Gloss
    import Control.Concurrent
    import Control.Concurrent.STM
    
    import Entities.Game
    import Entities.Score
    import Entities.Spawner
    
    width, height, offset :: Int
    width = 500
    height = 500
    offset = 0

    window:: Display
    window = InWindow "Runner" (width, height) (offset, offset)

    background :: Color
    background = black

    render :: (Game, Score, Obstacles) -> IO(Picture)
    render (g, s, o) = do
        score <- readMVar s
        obstacles <- atomically(readTVar o)
        return (pictures ([ renderFloor , renderScore score, renderPlayer (player g)] ++  map (renderObstacle) (obstacles)))

    renderPlayer :: (Float, Float) -> Picture
    renderPlayer (x, y) = translate (x) (y) $ color red $ circleSolid 20

    renderObstacle :: (Float, Float) -> Picture
    renderObstacle (x, y) = translate (x) (y) $ color blue $ rectangleSolid 10 30

    renderScore :: Int -> Picture
    renderScore score = translate (175) (175) $ scale (0.2) (0.2) $ (Text (show score))
    
    renderFloor :: Picture
    renderFloor = (color green $ (Line [(-500, 0), (500, 0)]))
