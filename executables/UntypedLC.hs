module Main where

import HaskellCourse.UntypedLC.UntypedLC
import HaskellCourse.Util

-- | Run an AE program from a file passed in via the command line.
-- Print the results back to stdout.
main :: IO ()
main = runCode (show . runUntypedLC)
