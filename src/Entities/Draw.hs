
module Entities.Draw (render, background, window, width) where
    import Graphics.Gloss
    import Control.Concurrent
    import Control.Concurrent.STM
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
    background = makeColorI 168 255 255 150

    floorColor :: Color
    floorColor = makeColorI 185 156 107 255
  
    floorDetailColor :: Color
    floorDetailColor = makeColorI 213 196 161 255

    render :: State -> IO(Picture)
    render (g, s, o, d, go, obstPic, plyPic, playerPics, obstaclePics, playerFrame, obstacleFrame) = do
        score <- readMVar s
        obst <- readMVar o
        difficulty <- readMVar d
        gameOver <- readMVar go
        playerPic <- renderPlayerIO (plyPic) (player g)
        obstacles <- mapM (renderObstacleIO obstPic) (obst)
        if (gameOver) then return (renderGameOverScreen score)
        else do 
            return (pictures ([renderFloor , (renderDificulty difficulty), renderFloorDetails, renderSun, renderScore score, playerPic] ++ obstacles))

    renderPlayer :: (Float, Float) -> Picture
    renderPlayer (x, y) = translate (x) (y) $ color red $ circleSolid playerRadius

    renderPlayerIO :: Picture -> (Float, Float) -> IO(Picture)
    renderPlayerIO pic (x, y) = do
        return (translate (x) (y+5) $ scale 1.5 1.5 pic )

    renderObstacle :: (Float, Float) -> Picture
    renderObstacle (x, y) = translate (x) (y) $ color blue $ rectangleSolid obstacleWidth obstacleHeight
    
    renderObstacleIO :: Picture -> (Float, Float) -> IO(Picture)
    renderObstacleIO pic (x, y) = do
        return (translate (x) (y+10) $ scale 1.5 1.5 pic)

    renderScore :: Int -> Picture
    renderScore score = translate (80) (-175) $ scale (0.2) (0.2) $ (Text ("Score " ++ show score))
    
    renderDificulty :: Float -> Picture
    renderDificulty difficulty = translate (-175) (-175) $ scale (0.2) (0.2) $ (Text ("Difficulty " ++ (show difficulty)))

    renderFloor :: Picture
    renderFloor = (color floorColor $ (Polygon [(-500, 0), (500, 0), (500, -500), (-500, -500)]))
    
    renderSun :: Picture
    renderSun = translate (200) (200) $ color yellow $ circleSolid 25

    renderFloorDetails :: Picture
    renderFloorDetails = (color floorDetailColor $ (Polygon [(-500, 0), (500, 0), (500, -7), (-500, -7)]))
    
    renderGameOverScreen :: Int -> Picture
    renderGameOverScreen score = (pictures [ translate (-175) (0) $ scale (0.5) (0.5) $ (Text "Game Over"), renderPressSpaceKey, (renderScoreGameOver score)])
    
    renderPressSpaceKey :: Picture 
    renderPressSpaceKey = translate (-150) (-40) $ scale (0.15) (0.15) $(Text "Press r to restart the game") 
    
    renderScoreGameOver :: Int -> Picture
    renderScoreGameOver score = translate (-60) (-80) $ scale (0.15) (0.15) $ (Text ("Score: " ++  (show score)))