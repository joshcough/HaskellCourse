module HaskellCourse.AE.AE (
  module HaskellCourse.AE.AE
 ,module HaskellCourse.AE.AST
 ,module HaskellCourse.AE.Interp
 ,module HaskellCourse.AE.Parser
 ,module HaskellCourse.Parsing
) where

import HaskellCourse.AE.AST
import HaskellCourse.AE.Interp
import HaskellCourse.AE.Parser
import HaskellCourse.Parsing

-- | Parse and interpret the given AE code.
runAE :: String -> Int
runAE = interp . parseExp . readSExpr 

