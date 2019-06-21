
module Entities.Draw (render, background, window, width) where
    import Graphics.Gloss
    import Control.Concurrent
    import Control.Concurrent.STM
    import System.Directory
    import Control.Monad

    import Entities.Game
    import Entities.Types
    
    width, height, offset :: Int
    width = 500
    height = 500
    offset = 0

    window:: Display
    window = InWindow "Runner" (width, height) (offset, offset)

    background :: Color
    background = makeColorI 247 247 247 1

    floorColor :: Color
    floorColor = makeColorI 0 0 0 1

    render :: State -> IO(Picture)
    render (g, s, o, d, go) = do
        score <- readMVar s
        obst <- readMVar o
        dificulty <- readMVar d
        gameOver <- readMVar go
        playerPic <- renderPlayerIO (player g)
        obstacles <- mapM (renderObstacleIO) (obst)
        if (gameOver) then return (renderGameOverScreen score)
        else do 
            return (pictures ([ (renderDificulty dificulty), renderFloor , renderScore score, playerPic] ++ obstacles))

    renderPlayer :: (Float, Float) -> Picture
    renderPlayer (x, y) = translate (x) (y) $ color red $ circleSolid playerRadius
    
    renderPlayerIO :: (Float, Float) -> IO(Picture)
    renderPlayerIO (x, y) = do
        dir <- getCurrentDirectory
        pic <- loadBMP (dir ++ "\\images\\dino.bmp")
        return (translate (x) (y+5) $ scale 0.2 0.2 pic )

    renderObstacle :: (Float, Float) -> Picture
    renderObstacle (x, y) = translate (x) (y) $ color blue $ rectangleSolid obstacleWidth obstacleHeight
    
    renderObstacleIO :: (Float, Float) -> IO(Picture)
    renderObstacleIO (x, y) = do
        dir <- getCurrentDirectory
        pic <- loadBMP (dir ++ "\\images\\cactus.bmp")
        return (translate (x) (y+5) $ scale 0.2 0.2 pic )

    renderScore :: Int -> Picture
    renderScore score = translate (175) (175) $ scale (0.2) (0.2) $ (Text (show score))
    
    renderDificulty :: Float -> Picture
    renderDificulty dificulty = translate (-175) (175) $ scale (0.2) (0.2) $ (Text ("Dificulty: " ++ (show dificulty)))

    renderFloor :: Picture
    renderFloor = (color (greyN 0.8) $ (Line [(-500, 0), (500, 0)]))

    renderGameOverScreen :: Int -> Picture
    renderGameOverScreen score = (pictures [ translate (-175) (0) $ scale (0.5) (0.5) $ (Text "Game Over"), renderPressSpaceKey, (renderScoreGameOver score)])
    
    renderPressSpaceKey :: Picture 
    renderPressSpaceKey = translate (-150) (-40) $ scale (0.15) (0.15) $(Text "Press r to restart the game") 
    
    renderScoreGameOver :: Int -> Picture
    renderScoreGameOver score = translate (-60) (-80) $ scale (0.15) (0.15) $ (Text ("Score: " ++  (show score)))