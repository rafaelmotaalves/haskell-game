module Entities.Dificulty where
    import Control.Concurrent
    import Entities.Types

    newDificulty :: IO (Dificulty)
    newDificulty = newMVar 1

    raiseDificultyAndRestartGame :: Score -> Obstacles -> Dificulty -> IO ()
    raiseDificultyAndRestartGame score obstacles dificulty =do
        dificultyLevel <- takeMVar dificulty
        putMVar dificulty (succ dificultyLevel)
      
        obs <- takeMVar obstacles
        putMVar obstacles ([])
      
        s <- takeMVar score
        putMVar score (0)

    handleDificultyRaise :: State -> IO (State)
    handleDificultyRaise (game, score, obstacles, dificulty) = do
        raiseDificultyAndRestartGame score obstacles dificulty
        return (game, score, obstacles, dificulty)
