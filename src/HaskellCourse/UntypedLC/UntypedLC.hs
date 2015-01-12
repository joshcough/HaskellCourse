module HaskellCourse.UntypedLC.UntypedLC (
  module HaskellCourse.UntypedLC.UntypedLC
 ,module HaskellCourse.UntypedLC.AST
 ,module HaskellCourse.UntypedLC.Parser
 ,module HaskellCourse.UntypedLC.StrictInterp
 ,module HaskellCourse.Parsing
) where

import Data.Map (Map)
import qualified Data.Map as Map
import HaskellCourse.UntypedLC.AST
import HaskellCourse.UntypedLC.Parser
import HaskellCourse.Parsing

import HaskellCourse.UntypedLC.StrictInterp
import qualified HaskellCourse.UntypedLC.StrictInterp as SI

runUntypedLC :: String -> SI.Value
runUntypedLC s = SI.interp (parseExp $ readSExpr s) Map.empty

