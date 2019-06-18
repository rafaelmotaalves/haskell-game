module Entities.Spawner where
    
    newObstacle :: (Float, Float)
    newObstacle = (500,15)

    timeBetweenSpawn :: Float
    timeBetweenSpawn = 5

    spawn :: Float -> [(Float, Float)] -> [(Float, Float)]
    spawn seconds obstaclesList = if (True) then (newObstacle:obstaclesList) else obstaclesList