module Entities.Spawner where
    import Control.Concurrent
    import Entities.Types    
    
    newObstacle :: (Float, Float)
    newObstacle = (260,15)

    timeBetweenSpawn :: Int
    timeBetweenSpawn = 2000000  -- 2 seconds

    spawn :: Obstacles -> IO()
    spawn obstaclesList = do
        currObstacles <- takeMVar obstaclesList
        putMVar obstaclesList (newObstacle:currObstacles)
        threadDelay timeBetweenSpawn
        spawn obstaclesList