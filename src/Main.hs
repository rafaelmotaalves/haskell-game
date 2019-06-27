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
stepGame _ (game, score, obstacles, dificulty, gameOver, obstaclePic, playerPic, playerPics, obstaclePics, playerFrame, obstacleFrame) = do
  go <- readMVar gameOver

  if (not go) 
    then do
    d <- readMVar dificulty
    o <- takeMVar obstacles
    
    putMVar obstacles (moveAllObstacles o d)
    
    if (hasCollision (player game) o)
    then do 
            setGameOver gameOver (True)
            return (newGame, score, obstacles, dificulty, gameOver, obstaclePic, playerPic, playerPics, obstaclePics, playerFrame, obstacleFrame)
    else do return (Game { 
      player = (adjustHeight (inJump game) (player game)),
      inJump = ((inJump game) && not (reachedMaxHeight (player game))),
      completedJump = (finishedJump (player game))
    }, score, obstacles, dificulty, gameOver, obstaclePics!!obstacleFrame, playerPics!!playerFrame, playerPics, obstaclePics, (playerFrame+1)`mod`(kingFrames-1), (obstacleFrame+1)`mod`(enemyFrames-1))
    
  else do 
    return (game, score, obstacles, dificulty, gameOver, obstaclePic, playerPic, playerPics, obstaclePics, playerFrame, obstacleFrame) -- do nothing


loadImages :: String -> [Picture] -> Int -> Int -> IO([Picture])
loadImages char a i m = do
  if i <= m then do
    dir <- getCurrentDirectory
    pic <- loadBMP (dir ++ "/images/" ++ char ++ "/" ++ show i ++ ".bmp")
    loadImages char (a ++ [pic] ++ [pic] ++ [pic] ++ [pic]) (i+1) m
  else return (a)

kingFrames :: Int
kingFrames = 17

enemyFrames :: Int
enemyFrames = 17

main :: IO ()
main = do
  score <- newMVar 0
  obstacles <- newObstacles
  dificulty <- newDificulty
  gameOver <- newMVar False
  
  dir <- getCurrentDirectory

  kingPics <- loadImages "king" [] 1 kingFrames
  enemyPics <- loadImages "enemy" [] 1 enemyFrames

  cactusPic <- loadBMP (dir ++ "/images/cactus.bmp")
  dinoPic <- loadBMP (dir ++ "/images/dino.bmp")
  
  forkIO $ (increaseDifficulty dificulty)
  forkIO $ (scoreIncrementer score dificulty gameOver) 
  forkIO $ (spawn obstacles) 
  
  playIO
    window
    background
    fps
    (newGame, score, obstacles, dificulty, gameOver, kingPics!!0, enemyPics!!0, kingPics, enemyPics, 0, 0)
    render
    handleInput
    stepGame


