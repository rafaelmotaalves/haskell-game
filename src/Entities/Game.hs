
module Entities.Game where
    import Control.Concurrent

    import Entities.Types

    playerRadius :: Float
    playerRadius = 20

    obstacleWidth :: Float
    obstacleWidth = 20

    obstacleHeight :: Float
    obstacleHeight = 30
    
    defaultPlayerPos :: (Float, Float)
    defaultPlayerPos = (0, 0)

    newGame :: Game
    newGame = Game { player = defaultPlayerPos, inJump = False, completedJump = True }

    resetStateButKeepDificulty :: State -> IO ()
    resetStateButKeepDificulty (game, score, obstacles, dificulty, gameOver, obstaclePic, playerPic ) = do 
        obs <- takeMVar obstacles
        putMVar obstacles ([])
  
        s <- takeMVar score
        putMVar score (0)

        d <- takeMVar dificulty

        putMVar dificulty (fromIntegral (floor d))

        return ()

    geometricDistance :: (Float, Float) -> (Float, Float) -> Float
    geometricDistance (x1, y1) (x2, y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

    testCollision :: (Float, Float) -> (Float, Float) -> Bool
    testCollision a b = xCollison && yCollision
        where xCollison = distance < playerRadius + (obstacleWidth / 2)
              yCollision = distance < playerRadius + (obstacleHeight / 2)
              distance = (geometricDistance a b)
            
    hasCollision :: (Float, Float) -> [(Float, Float)] -> Bool
    hasCollision playerPos obstacles = (any (testCollision (playerPos)) obstacles)

    setGameOver :: GameOver -> Bool -> IO ()
    setGameOver gOver value = do
        status <- takeMVar gOver
        putMVar gOver value

    handleRestartGame :: State -> IO (State)
    handleRestartGame (game, score, obstacles, dificulty, gameOver, obstaclePic, playerPic) = do
        setGameOver gameOver False 
        resetStateButKeepDificulty (game, score, obstacles, dificulty, gameOver, obstaclePic, playerPic)
        return (game, score, obstacles, dificulty, gameOver, obstaclePic, playerPic)