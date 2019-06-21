
module Entities.Draw (render, background, window, width) where
    import Graphics.Gloss
    import Control.Concurrent
    import Control.Concurrent.STM
    
    import Entities.Game
    import Entities.Types
    
    width, height, offset :: Int
    width = 500
    height = 500
    offset = 0

    window:: Display
    window = InWindow "Runner" (width, height) (offset, offset)

    background :: Color
    background = black

    render :: State -> IO(Picture)
    render (g, s, o, d, go) = do
        score <- readMVar s
        obstacles <- readMVar o
        dificulty <- readMVar d
        gameOver <- readMVar go

        if (gameOver) then return renderGameOverScreen
        else do return (pictures ([ (renderDificulty dificulty), renderFloor , renderScore score, renderPlayer (player g)] ++  map (renderObstacle) (obstacles)))

    renderPlayer :: (Float, Float) -> Picture
    renderPlayer (x, y) = translate (x) (y) $ color red $ circleSolid playerRadius

    renderObstacle :: (Float, Float) -> Picture
    renderObstacle (x, y) = translate (x) (y) $ color blue $ rectangleSolid obstacleWidth obstacleHeight

    renderScore :: Int -> Picture
    renderScore score = translate (175) (175) $ scale (0.2) (0.2) $ (Text (show score))
    
    renderDificulty :: Float -> Picture
    renderDificulty dificulty = translate (-175) (175) $ scale (0.2) (0.2) $ (Text ("Dificulty: " ++ (show dificulty)))

    renderFloor :: Picture
    renderFloor = (color green $ (Line [(-500, 0), (500, 0)]))

    renderGameOverScreen :: Picture
    renderGameOverScreen  = (pictures [ translate (-175) (0) $ scale (0.5) (0.5) $ (Text "Game Over"), renderPressSpaceKey])
    
    renderPressSpaceKey :: Picture 
    renderPressSpaceKey = translate (-150) (-40) $ scale (0.15) (0.15) $(Text "Press r to restart the game") 
