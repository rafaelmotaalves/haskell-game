module Main where

import Graphics.Gloss
import Entities.Game

background :: Color
background = white

main :: IO ()
main = display FullScreen background (Circle 90)


