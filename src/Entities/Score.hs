
module Entities.Score where
    import Control.Concurrent

    type Score = MVar Int

    scoreIncrementer :: Score -> IO ()
    scoreIncrementer scoreVar = do
        score <- takeMVar scoreVar
        putMVar scoreVar (score + 10)
        threadDelay 1000000 -- delay one second
        scoreIncrementer scoreVar