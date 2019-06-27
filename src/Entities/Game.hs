
module Entities.Game where
    import Control.Concurrent

    import Entities.Types

    playerRadius :: Float
    playerRadius = 20

    obstacleWidth :: Float
    obstacleWidth = 20

    obstacleHeight :: Float
    obstacleHeight = 30
    
    xCollisionDistance :: Float
    xCollisionDistance = playerRadius + (obstacleWidth / 2)

    yCollisionDistance :: Float
    yCollisionDistance = playerRadius + (obstacleHeight / 2)

    defaultPlayerPos :: (Float, Float)
    defaultPlayerPos = (0, 0)

    newGame :: Game
    newGame = Game { player = defaultPlayerPos, inJump = False, completedJump = True }

    resetStateButKeepDifficulty :: State -> IO ()
    resetStateButKeepDifficulty (game, score, obstacles, difficulty, gameOver, obstaclePic, playerPic, playerPics, obstaclePics, playerFrame, obstacleFrame ) = do 
        obs <- takeMVar obstacles
        putMVar obstacles ([])
  
        s <- takeMVar score
        putMVar score (0)

        d <- takeMVar difficulty

        putMVar difficulty (fromIntegral (floor d))

        return ()

    geometricDistanceSquared :: (Float, Float) -> (Float, Float) -> Float
    geometricDistanceSquared (x1, y1) (x2, y2) = ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))

    testCollision :: (Float, Float) -> (Float, Float) -> Bool
    testCollision a b = xCollison && yCollision
        where xCollison = distanceSquared < xCollisionDistance * xCollisionDistance
              yCollision = distanceSquared < yCollisionDistance * yCollisionDistance
              distanceSquared = (geometricDistanceSquared a b)
            
    hasCollision :: (Float, Float) -> [(Float, Float)] -> Bool
    hasCollision playerPos obstacles = (any (testCollision (playerPos)) obstacles)

    setGameOver :: GameOver -> Bool -> IO ()
    setGameOver gOver value = do
        status <- takeMVar gOver
        putMVar gOver value

    handleRestartGame :: State -> IO (State)
    handleRestartGame (game, score, obstacles, difficulty, gameOver, obstaclePic, playerPic, playerPics, obstaclePics, playerFrame, obstacleFrame) = do
        setGameOver gameOver False 
        resetStateButKeepDifficulty (game, score, obstacles, difficulty, gameOver, obstaclePic, playerPic, playerPics, obstaclePics, playerFrame, obstacleFrame)
        return (game, score, obstacles, difficulty, gameOver, obstaclePic, playerPic, playerPics, obstaclePics, playerFrame, obstacleFrame)