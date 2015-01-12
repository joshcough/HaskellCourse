module HaskellCourse.AE.Interp (interp) where

import HaskellCourse.AE.AST
import HaskellCourse.Prim

data Value = IntVal Int | BoolVal Bool

-- | Interpret the given Expression, producing an Int.
interp :: Exp -> Value
interp (LitInt  i) = ???
interp (LitBool b) = ??? 
interp (App p a b) = ???
  runPrim p (interp a) (interp b)
