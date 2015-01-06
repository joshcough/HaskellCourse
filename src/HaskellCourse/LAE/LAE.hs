module HaskellCourse.LAE.LAE (
  module HaskellCourse.LAE.LAE
 ,module HaskellCourse.LAE.AST
 ,module HaskellCourse.LAE.Interp
 ,module HaskellCourse.LAE.Parser
 ,module HaskellCourse.LAE.TypeChecker
 ,module HaskellCourse.Parsing
) where

import Data.Map (Map)
import qualified Data.Map as Map
import HaskellCourse.LAE.AST
import HaskellCourse.LAE.Interp
import HaskellCourse.LAE.Parser
import HaskellCourse.LAE.TypeChecker
import HaskellCourse.Parsing

-- | Parse, typecheck and interpret the given LAE code.
runLAE :: String -> Runtime
runLAE s = 
  let exp = parseExp $ readSExpr s
      typ = typeCheck exp Map.empty
  in if isRunnable typ 
     then interp exp Map.empty
     else error $ "unrunnable expression, with type: " ++ show typ

-- | Returns True if the given type is BoolT or IntT.
isRunnable :: Type -> Bool
isRunnable t = t `elem` [BoolT, IntT]

