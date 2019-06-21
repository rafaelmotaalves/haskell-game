
module Entities.Score where
    import Control.Concurrent
    import Entities.Types
    import Control.Monad

    scoreIncrementer :: Score -> Dificulty -> GameOver -> IO ()
    scoreIncrementer scoreVar dificultyVar gameOverVar = do
        score <- takeMVar scoreVar
        dificulty <- readMVar dificultyVar
        gameOver <- readMVar gameOverVar

        when (not gameOver) $ (putMVar scoreVar (score + 10))
        when (gameOver) $ (putMVar scoreVar score)
        
        threadDelay (round (1000000 / dificulty))
        scoreIncrementer scoreVar dificultyVar gameOverVar