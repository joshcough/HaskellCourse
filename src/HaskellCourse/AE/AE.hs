module HaskellCourse.AE.AE (
  module HaskellCourse.AE.AE
 ,module HaskellCourse.AE.AST
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

-- | Parse, typecheck and interpret the given AE code.
runAE :: String -> Runtime
runAE s = 
  let exp = parseExp $ readSExpr s
      typ = typeCheck exp
  in if isRunnable typ 
     then interp exp
     else error $ "unrunnable expression, with type: " ++ show typ

-- | Returns True if the given type is BoolT or IntT.
isRunnable :: Type -> Bool
isRunnable t = t `elem` [BoolT, IntT]

