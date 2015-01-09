{-# Language BangPatterns #-}

module HaskellCourse.UntypedLC.Interp (interp, Runtime(..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import HaskellCourse.Prim
import HaskellCourse.UntypedLC.AST

data Runtime = IntR Int | Closure Var Exp Env deriving (Show)

type Env = Map Var Runtime

interp :: Exp -> Env -> Runtime
interp (LitInt  i)     _   = IntR i
interp (Var     v)     env = fromMaybe (error $ "unbound var: " ++ v) (Map.lookup v env)
interp (Let v e b)     env = let !re = interp e env in interp b (Map.insert v re env) 
interp (PrimApp p a b) env = 
  let IntR l = interp a env
      IntR r = interp b env
  in let !res = runPrim p l r in IntR res
interp (Lambda v b)    env = Closure v b env
interp (App f a)       env =
  let (Closure v exp env') = interp f env
      !arg                 = interp a env
  in interp exp (Map.insert v arg env') 
