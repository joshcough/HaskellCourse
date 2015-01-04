module AEMain where

import HaskellCourse.AE.AE
import System.Environment

-- | Run an AE program from a file passed in via the command line.
-- Print the results back to stdout.
main :: IO ()
main = do
  args <- getArgs
  code <- readFile (args !! 0)
  let res = runAE code
  putStrLn $ show res
