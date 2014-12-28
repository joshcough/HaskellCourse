module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  code <- readFile $ args !! 0
  putStrLn code

