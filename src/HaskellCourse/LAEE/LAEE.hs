module HaskellCourse.LAEE.LAEE (
  module HaskellCourse.LAEE.LAEE
 ,module HaskellCourse.LAEE.AST
 ,module HaskellCourse.LAEE.Interp
 ,module HaskellCourse.LAEE.Parser
 ,module HaskellCourse.Parsing
) where

import Data.Map (Map)
import qualified Data.Map as Map
import HaskellCourse.LAEE.AST
import HaskellCourse.LAEE.Interp
import HaskellCourse.LAEE.Parser
import HaskellCourse.Parsing

-- | Parse, typecheck and interpret the given LAE code.
runLAEE :: String -> Int
runLAEE s = interp (parseExp $ readSExpr s) Map.empty

