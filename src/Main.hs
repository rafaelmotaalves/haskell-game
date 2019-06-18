module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

import Entities.Game
import Entities.Draw
import Entities.Player

handleInput :: Event -> Game -> IO (Game)
handleInput (EventKey (Char 'w') (Down) _ _) game = do
  return Game { player = (player game), obstacles = (obstacles game), inJump = (True)  }

handleInput _ g = return g

stepGame :: Float -> Game -> IO (Game)
stepGame _ game = do
  return Game { player = (adjustHeight (inJump game) (player game)), obstacles = (obstacles game), inJump = (toggleJump (inJump game) (player game)) }

main :: IO ()
main = do
  playIO
    (InWindow "J-Rex" (500, 500) (500, 500))
    white
    30
    restartGame
    render
    handleInput
    stepGame


