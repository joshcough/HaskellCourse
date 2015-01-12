{-# Language BangPatterns #-}

module HaskellCourse.UntypedLC.StrictInterp (interp, Value(..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import HaskellCourse.Prim
import HaskellCourse.UntypedLC.AST

data Value = IntV Int | Closure Var Exp Env deriving (Show)

type Env = Map Var Value

interp :: Exp -> Env -> Value
interp (LitInt  i)     _   = IntV i
interp (Var     v)     env = fromMaybe (error $ "unbound var: " ++ v) (Map.lookup v env)
interp (Let v e b)     env = let !re = interp e env in interp b (Map.insert v re env) 
interp (PrimApp p a b) env = 
  let IntV l = interp a env
      IntV r = interp b env
  in let !res = runPrim p l r in IntV res
interp (Lambda v b)    env = Closure v b env
interp (App f a)       env =
  let (Closure v exp env') = interp f env
      !arg                 = interp a env
  in interp exp (Map.insert v arg env') 
