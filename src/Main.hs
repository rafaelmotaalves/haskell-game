module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

import Entities.Game
import Entities.Draw
import Entities.Player
import Entities.Spawner
import Entities.Obstacle

fps :: Int
fps = 30

handleInput :: Event -> Game -> IO (Game)
handleInput (EventKey (Char 'w') (Down) _ _) game = do
  return Game { player = (player game), obstacles = (obstacles game), inJump = ((completedJump game) && True), completedJump = False }

handleInput _ g = return g

stepGame :: Float -> Game -> IO (Game)
stepGame seconds game = do
  return Game { 
    player = (adjustHeight (inJump game) (player game)), 
    obstacles =(map (moveLeft) $ spawn seconds (obstacles game)), 
    inJump = ((inJump game) && not (reachedMaxHeight (player game))),
    completedJump = (finishedJump (player game))
  }

main :: IO ()
main = do
  playIO
    window
    white
    fps
    restartGame
    render
    handleInput
    stepGame


