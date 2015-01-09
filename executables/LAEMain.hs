module LAEMain where

import HaskellCourse.LAEE.LAEE
import HaskellCourse.Util

-- | Run an AE program from a file passed in via the command line.
-- Print the results back to stdout.
main :: IO ()
main = runCode (show . runLAEE)

