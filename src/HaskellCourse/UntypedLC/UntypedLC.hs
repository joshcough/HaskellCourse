module HaskellCourse.UntypedLC.UntypedLC (
  module HaskellCourse.UntypedLC.UntypedLC
 ,module HaskellCourse.UntypedLC.AST
 ,module HaskellCourse.UntypedLC.Interp
 ,module HaskellCourse.UntypedLC.Parser
 ,module HaskellCourse.Parsing
) where

import Data.Map (Map)
import qualified Data.Map as Map
import HaskellCourse.UntypedLC.AST
import HaskellCourse.UntypedLC.Interp
import HaskellCourse.UntypedLC.Parser
import HaskellCourse.Parsing

runUntypedLC :: String -> Runtime
runUntypedLC s = interp (parseExp $ readSExpr s) Map.empty

