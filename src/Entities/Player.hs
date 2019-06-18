module Entities.Player where

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

    -- checks if its on the limit of the jump
    toggleJump :: Bool -> (Float, Float) -> Bool
    toggleJump inJump pos = inJump && (snd pos) < jumpLimit