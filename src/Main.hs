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

handleInput :: Event -> State -> IO State
handleInput (EventKey (Char 'w') (Down) _ _) (game, score, obstacles, dificulty) = do
  return (Game { player = (player game), inJump = ((completedJump game) && True), completedJump = False }, score, obstacles, dificulty)

handleInput (EventKey (Char 'd') (Down) _ _ ) (game, score, obstacles, dificulty) = do
  dificultyLevel <- takeMVar dificulty
  putMVar dificulty (dificultyLevel + 1)

  obs <- takeMVar obstacles
  putMVar obstacles ([])

  s <- takeMVar score
  putMVar score (0)
  
  return (game, score, obstacles, dificulty)

handleInput _ g = return g

stepGame :: Float -> State -> IO State
stepGame _ (game, score, obstacles, dificulty) = do
  t <- readMVar dificulty
  o <- takeMVar obstacles
  putMVar obstacles (filter atScreen $ map (moveLeft t) o)
  return (Game { 
    player = (adjustHeight (inJump game) (player game)),
    inJump = ((inJump game) && not (reachedMaxHeight (player game))),
    completedJump = (finishedJump (player game))
  }, score, obstacles, dificulty)


main :: IO ()
main = do
  score <- newMVar 0
  obstacles <- newMVar []
  dificulty <- newMVar 1

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


