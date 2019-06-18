module Main where

import Control.Concurrent

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

import Entities.Game
import Entities.Draw
import Entities.Player
import Entities.Spawner
import Entities.Obstacle
import Entities.Score

fps :: Int
fps = 30

handleInput :: Event -> (Game, Score) -> IO (Game, Score)
handleInput (EventKey (Char 'w') (Down) _ _) (game, score) = do
  return (Game { player = (player game), obstacles = (obstacles game), inJump = ((completedJump game) && True), completedJump = False }, score)

handleInput _ g = return g

stepGame :: Float -> (Game, Score) -> IO (Game, Score)
stepGame seconds (game, score) = do
  return (Game { 
    player = (adjustHeight (inJump game) (player game)), 
    obstacles =(map (moveLeft) $ spawn seconds (obstacles game)), 
    inJump = ((inJump game) && not (reachedMaxHeight (player game))),
    completedJump = (finishedJump (player game))
  }, score)


main :: IO ()
main = do
  score <- newMVar 0
  forkIO $ (scoreIncrementer score) 
  playIO
    window
    white
    fps
    (restartGame, score)
    render
    handleInput
    stepGame


