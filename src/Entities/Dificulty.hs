module Entities.Dificulty where
    import Control.Concurrent
    import Entities.Types

    newDificulty :: IO (Dificulty)
    newDificulty = newMVar 1

    normalizeDifficulty :: Float -> Float
    normalizeDifficulty d = max 1 d

    raiseDificultyAndRestartGame :: Score -> Obstacles -> Dificulty -> (Float -> Float) -> IO ()
    raiseDificultyAndRestartGame score obstacles dificulty incrementalFunc = do
        dificultyLevel <- takeMVar dificulty
        
        putMVar dificulty (min (normalizeDifficulty $ incrementalFunc dificultyLevel) maxDifficulty)
      
        obs <- takeMVar obstacles
        putMVar obstacles ([])
      
        s <- takeMVar score
        putMVar score (0)

    handleDificultyRaise :: State -> (Float -> Float) -> IO (State)
    handleDificultyRaise (game, score, obstacles, dificulty, gameOver) incrementalFunc = do
        raiseDificultyAndRestartGame score obstacles dificulty incrementalFunc
        return (game, score, obstacles, dificulty, gameOver)
    
    increaseRate :: Float
    increaseRate = 0.1

    maxDifficulty :: Float
    maxDifficulty = 3

    increaseDelay :: Int
    increaseDelay = 10000000 -- 10 seconds

    increaseDifficulty :: Dificulty -> IO()
    increaseDifficulty difficulty = do
        d <- readMVar difficulty
        if (d >= maxDifficulty) then 
            return ()
        else do
            d <- takeMVar difficulty
            putMVar difficulty (d+increaseRate)
            threadDelay increaseDelay
            increaseDifficulty difficulty

