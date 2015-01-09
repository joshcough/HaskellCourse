module HaskellCourse.Util where

import Data.List
import System.Environment

list :: [String] -> String
list as = "(" ++ spaced as ++ ")"

spaced :: [String] -> String
spaced as = concat $ intersperse " " as

runCode :: (String -> String) -> IO ()
runCode f = do
  args <- getArgs
  code <- readFile (args !! 0)
  putStrLn $ f code
