
module Entities.Score where
    import Control.Concurrent
    
    type Score = MVar Int

    scoreIncrementer :: Score -> MVar Float -> IO ()
    scoreIncrementer scoreVar dificultyVar= do
        score <- takeMVar scoreVar
        dificulty <- readMVar dificultyVar
        putMVar scoreVar (score + 10)
        threadDelay (round (1000000 / dificulty))
        scoreIncrementer scoreVar dificultyVar