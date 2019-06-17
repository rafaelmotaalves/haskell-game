module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

import Entities.Game
import Entities.Draw

handleInput :: Event -> Game -> IO (Game)
handleInput (EventKey (Char 'w') (Down) _ _) game = return Game { player = (0, 100), obstacles = (obstacles game) }
handleInput _ g = return g

stepGame :: Float -> Game -> IO (Game)
stepGame _ game = return Game { player = defaultPlayerPos, obstacles = (obstacles game)}

main :: IO ()
main = do
  playIO
    (InWindow "J-Rex" (500, 500) (500, 500))
    white
    10
    restartGame
    render
    handleInput
    stepGame


