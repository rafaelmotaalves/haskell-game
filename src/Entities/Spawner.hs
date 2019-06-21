module Entities.Spawner where
    import Control.Concurrent
    import Entities.Types    
    import System.Random

    newObstacle :: (Float, Float)
    newObstacle = (260,15)

    timeBetweenSpawn :: IO(Int)
    timeBetweenSpawn = randomRIO(500000, 2500000)-- 0.5 / 5 seconds

    spawn :: Obstacles -> IO()
    spawn obstaclesList = do
        currObstacles <- takeMVar obstaclesList
        putMVar obstaclesList (newObstacle:currObstacles)
        cooldown <- timeBetweenSpawn
        threadDelay cooldown
        spawn obstaclesList