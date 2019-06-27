
module Entities.Score where
    import Control.Concurrent
    import Entities.Types
    import Control.Monad

    scoreIncrementer :: Score -> Difficulty -> GameOver -> IO ()
    scoreIncrementer scoreVar difficultyVar gameOverVar = do
        score <- takeMVar scoreVar
        difficulty <- readMVar difficultyVar
        gameOver <- readMVar gameOverVar

        when (not gameOver) $ (putMVar scoreVar (score + 10))
        when (gameOver) $ (putMVar scoreVar score)
        
        threadDelay (round (1000000 / difficulty))
        scoreIncrementer scoreVar difficultyVar gameOverVar