{-# Language BangPatterns #-}

module HaskellCourse.LAEE.Interp (interp) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import HaskellCourse.Prim
import HaskellCourse.LAEE.AST

type Env = Map Var Int

-- | Interpret the given Expression, producing an Int.
interp :: Exp -> Env -> Int
interp (LitInt  i)     _   = i
interp (Var     v)     env = fromMaybe (error $ "unbound var: " ++ v) (Map.lookup v env)
interp (Let v e b)     env = let !re = interp e env in interp b (Map.insert v re env) 
interp (PrimApp p a b) env = runPrim p (interp a env) (interp b env)

