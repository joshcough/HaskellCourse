module HaskellCourse.AE.Interp (interp) where

import HaskellCourse.AE.AST
import HaskellCourse.Prim

-- | Interpret the given Expression, producing an Int.
interp :: Exp -> Int
interp (LitInt  i) = i
interp (App p a b) = runPrim p (interp a) (interp b)
