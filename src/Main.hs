module Main where

import Control.Concurrent
import Control.Concurrent.STM

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

import Entities.Game
import Entities.Draw
import Entities.Player
import Entities.Spawner
import Entities.Obstacle
import Entities.Score
import Entities.Dificulty
import Entities.Types

fps :: Int
fps = 30

handleInput :: Event -> State -> IO State

handleInput (EventKey (Char 'w') (Down) _ _) state = (handleJump state)

handleInput (EventKey (Char 'd') (Down) _ _ ) state = (handleDificultyRaise state)

handleInput _ g = return g

stepGame :: Float -> State -> IO State
stepGame _ (game, score, obstacles, dificulty) = do
  
  d <- readMVar dificulty
  o <- takeMVar obstacles
  
  putMVar obstacles (moveAllObstacles o d)

  return (Game { 
    player = (adjustHeight (inJump game) (player game)),
    inJump = ((inJump game) && not (reachedMaxHeight (player game))),
    completedJump = (finishedJump (player game))
  }, score, obstacles, dificulty)


main :: IO ()
main = do
  score <- newMVar 0
  obstacles <- newObstacles
  dificulty <- newDificulty

  forkIO $ (scoreIncrementer score dificulty) 
  forkIO $ (spawn obstacles) 
  playIO
    window
    white
    fps
    (restartGame, score, obstacles, dificulty)
    render
    handleInput
    stepGame


