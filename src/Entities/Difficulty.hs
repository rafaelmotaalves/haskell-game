module Entities.Difficulty where
    import Control.Concurrent
    import Entities.Types

    newDifficulty :: IO (Difficulty)
    newDifficulty = newMVar 1

    normalizeDifficulty :: Float -> Float
    normalizeDifficulty d = max 1 d

    raiseDifficultyAndRestartGame :: Score -> Obstacles -> Difficulty -> (Float -> Float) -> IO ()
    raiseDifficultyAndRestartGame score obstacles difficulty incrementalFunc = do
        difficultyLevel <- takeMVar difficulty
        
        putMVar difficulty (min (normalizeDifficulty $ incrementalFunc difficultyLevel) maxDifficulty)
      
        obs <- takeMVar obstacles
        putMVar obstacles ([])
      
        s <- takeMVar score
        putMVar score (0)

    handleDifficultyRaise :: State -> (Float -> Float) -> IO (State)
    handleDifficultyRaise (game, score, obstacles, difficulty, gameOver, obstaclePic, playerPic, playerPics, obstaclePics, playerFrame, obstacleFrame) incrementalFunc = do
        raiseDifficultyAndRestartGame score obstacles difficulty incrementalFunc
        return (game, score, obstacles, difficulty, gameOver, obstaclePic, playerPic, playerPics, obstaclePics, playerFrame, obstacleFrame)
    
    increaseRate :: Float
    increaseRate = 0.1

    maxDifficulty :: Float
    maxDifficulty = 3

    increaseDelay :: Int
    increaseDelay = 10000000 -- 10 seconds

    increaseDifficulty :: Difficulty -> IO()
    increaseDifficulty difficulty = do
        d <- readMVar difficulty
        if (d >= maxDifficulty) then 
            return ()
        else do
            d <- takeMVar difficulty
            putMVar difficulty (d+increaseRate)
            threadDelay increaseDelay
            increaseDifficulty difficulty

