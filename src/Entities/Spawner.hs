module Entities.Spawner where

    import Control.Concurrent
    import Control.Concurrent.STM
    
    type Obstacles = TVar [(Float, Float)]
    
    newObstacle :: (Float, Float)
    newObstacle = (260,15)

    timeBetweenSpawn :: Int
    timeBetweenSpawn = 2000000  -- 2 seconds

    spawn :: Obstacles -> IO()
    spawn obstaclesList = do
        currObstacles <- atomically(readTVar obstaclesList)
        atomically (writeTVar obstaclesList (newObstacle:currObstacles))
        threadDelay timeBetweenSpawn
        spawn obstaclesList