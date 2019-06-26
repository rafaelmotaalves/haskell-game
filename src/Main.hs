module Main where

import System.Directory

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

handleInput (EventKey (SpecialKey KeySpace) (Down) _ _) state = (handleJump state)

handleInput (EventKey (SpecialKey KeyPageUp) (Down) _ _ ) state = (handleDificultyRaise state succ)

handleInput (EventKey (SpecialKey KeyPageDown) (Down) _ _ ) state = (handleDificultyRaise state pred)

handleInput (EventKey (Char 'r') (Down) _ _ ) state = (handleRestartGame state)

handleInput _ g = return g

stepGame :: Float -> State -> IO State
stepGame _ (game, score, obstacles, dificulty, gameOver, obstaclePic, playerPic) = do
  go <- readMVar gameOver

  if (not go) 
    then do
    d <- readMVar dificulty
    o <- takeMVar obstacles
    
    putMVar obstacles (moveAllObstacles o d)
    
    if (hasCollision (player game) o)
    then do 
            setGameOver gameOver (True)
            return (newGame, score, obstacles, dificulty, gameOver, obstaclePic, playerPic)
    else do return (Game { 
      player = (adjustHeight (inJump game) (player game)),
      inJump = ((inJump game) && not (reachedMaxHeight (player game))),
      completedJump = (finishedJump (player game))
    }, score, obstacles, dificulty, gameOver, obstaclePic, playerPic)
    
  else do 
    return (game, score, obstacles, dificulty, gameOver, obstaclePic, playerPic) -- do nothing


loadImages :: [Picture] -> Int -> Int -> IO([Picture])
loadImages a i m = do
  if i <= m then do
    dir <- getCurrentDirectory
    pic <- loadBMP (dir ++ "/images/king/" ++ show i ++ ".bmp")
    loadImages (a ++ [pic]) (i+1) m
  else return (a)

main :: IO ()
main = do
  score <- newMVar 0
  obstacles <- newObstacles
  dificulty <- newDificulty
  gameOver <- newMVar False
  
  dir <- getCurrentDirectory

  pictures <- loadImages [] 1 18

  cactusPic <- loadBMP (dir ++ "/images/cactus.bmp")
  dinoPic <- loadBMP (dir ++ "/images/dino.bmp")
  
  forkIO $ (increaseDifficulty dificulty)
  forkIO $ (scoreIncrementer score dificulty gameOver) 
  forkIO $ (spawn obstacles) 
  
  playIO
    window
    background
    fps
    (newGame, score, obstacles, dificulty, gameOver, cactusPic, dinoPic)
    render
    handleInput
    stepGame


