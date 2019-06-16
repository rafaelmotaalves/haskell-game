module Main where

import Graphics.Gloss
import Entities.Game
import Entities.Draw

main :: IO ()
main = display window background (render restartGame) 


