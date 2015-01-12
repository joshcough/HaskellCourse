module HaskellCourse.LAE.LAE (
  module HaskellCourse.LAE.LAE
 ,module HaskellCourse.LAE.AST
 ,module HaskellCourse.LAE.Interp
 ,module HaskellCourse.LAE.Parser
 ,module HaskellCourse.Parsing
) where

import Data.Map (Map)
import qualified Data.Map as Map
import HaskellCourse.LAE.AST
import HaskellCourse.LAE.Interp
import HaskellCourse.LAE.Parser
import HaskellCourse.Parsing

-- | Parse, typecheck and interpret the given LAE code.
runLAE :: String -> Int
runLAE s = interp (parseExp $ readSExpr s) Map.empty

