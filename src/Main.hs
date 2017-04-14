module Main where

import Board

main :: IO ()
main = playGame

playGame :: IO ()
playGame = do
  putStrLn "We're playing a game!"
  putStrLn $ show newBoard
