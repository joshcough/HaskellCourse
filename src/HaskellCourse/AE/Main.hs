module HaskellCourse.AE.Main (
  module HaskellCourse.AE.AST
 ,module HaskellCourse.AE.Interp
 ,module HaskellCourse.AE.Parser
 ,module HaskellCourse.AE.TypeChecker
 ,module HaskellCourse.Parsing
) where

import HaskellCourse.AE.AST
import HaskellCourse.AE.Interp
import HaskellCourse.AE.Parser
import HaskellCourse.AE.TypeChecker
import HaskellCourse.Parsing

runAE :: String -> Runtime
runAE s = 
  let exp = parseString s
  in  seq (typeCheck exp) (interp exp)