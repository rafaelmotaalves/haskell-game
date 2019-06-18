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

fps :: Int
fps = 30

handleInput :: Event -> (Game, Score, Obstacles) -> IO (Game, Score, Obstacles)
handleInput (EventKey (Char 'w') (Down) _ _) (game, score, obstacles) = do
  return (Game { player = (player game), inJump = ((completedJump game) && True), completedJump = False }, score, obstacles)

handleInput _ g = return g

stepGame :: Float -> (Game, Score, Obstacles) -> IO (Game, Score, Obstacles)
stepGame _ (game, score, obstacles) = do
  o <- atomically(readTVar obstacles)
  atomically (writeTVar obstacles (filter atScreen $ map moveLeft o))
  return (Game { 
    player = (adjustHeight (inJump game) (player game)),
    inJump = ((inJump game) && not (reachedMaxHeight (player game))),
    completedJump = (finishedJump (player game))
  }, score, obstacles)


main :: IO ()
main = do
  score <- newMVar 0
  obstacles <- atomically(newTVar [])
  forkIO $ (scoreIncrementer score) 
  forkIO $ (spawn obstacles) 
  playIO
    window
    white
    fps
    (restartGame, score, obstacles)
    render
    handleInput
    stepGame


