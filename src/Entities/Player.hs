module Entities.Player where
    import Entities.Types
    jumpLimit :: Float
    jumpLimit = 100

    stepSize :: Float
    stepSize = 10

    baseHeight :: Float
    baseHeight = 20

    adjustHeight :: Bool -> (Float, Float) -> (Float, Float)
    adjustHeight inJump pos = (x, y)
        where x = fst pos
              y = if (inJump) 
              then (incrementHeight (snd pos))
              else (decreaseOrStayAtHeight (snd pos))

    incrementHeight :: Float -> Float
    incrementHeight y = y + stepSize

    decreaseOrStayAtHeight :: Float -> Float
    decreaseOrStayAtHeight y = if (y > baseHeight) then (y - stepSize) else (baseHeight)

    reachedMaxHeight :: (Float, Float) -> Bool
    reachedMaxHeight pos = (snd pos) >= jumpLimit

    finishedJump :: (Float, Float) -> Bool
    finishedJump pos = (snd pos) == baseHeight

    handleJump :: State -> IO (State)
    handleJump (game, score, obstacles, dificulty, gameOver, obstaclePic, playerPic, playerPics, obstaclePics, playerFrame, obstacleFrame) = 
        return (Game {
            player = (player game),
            inJump = ((completedJump game) && True), 
            completedJump = False 
            }, 
            score, 
            obstacles, 
            dificulty,
            gameOver, obstaclePic, playerPic, playerPics, obstaclePics, playerFrame, obstacleFrame)

