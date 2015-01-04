module HaskellCourse.AE.Main (
  module HaskellCourse.AE.AST
 ,module HaskellCourse.AE.Interp
 ,module HaskellCourse.AE.Parser
 ,module HaskellCourse.AE.TypeChecker
 ,module HaskellCourse.Parsing
 ,module HaskellCourse.AE.Main
) where

import HaskellCourse.AE.AST
import HaskellCourse.AE.Interp
import HaskellCourse.AE.Parser
import HaskellCourse.AE.TypeChecker
import HaskellCourse.Parsing
import System.Environment

runAE :: String -> Runtime
runAE s = let exp = parseExp $ readSExpr s
          in  seq (typeCheck exp) (interp exp)

main :: IO ()
main = do
  args <- getArgs
  code <- readFile (args !! 0)
  putStrLn $ show $ runAE code
