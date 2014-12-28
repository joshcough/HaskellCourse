module HaskellCourse.Lang1.Interp (interp) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import HaskellCourse.Lang1.AST

type Env = Map Variable Runtime
data Runtime = Num Int | Closure [Variable] E Env deriving (Show, Eq)

isTrue :: Runtime -> Bool
isTrue (Num 1) = True
isTrue _       = False 

interp :: E -> Env -> Runtime
interp (LitInt i)          env = Num i
interp (Var v)             env = envLookup v env
interp (Let v e body)      env = interp (App (Lambda [v] body) [e]) env
interp (IfStatement p t f) env = interpIf p t f env
interp (Lambda vs e)       env = Closure vs e env
interp (App f es)          env = interpApp f es env 
interp (PrimApp p e1 e2)   env = interpPrim p e1 e2 env

envLookup :: Variable -> Env -> Runtime
envLookup v env = fromMaybe (error $ "unbound variable: " ++ v) (Map.lookup v env)

interpIf :: E -> E -> E -> Env -> Runtime
interpIf p t f env = let p' = interp p env 
                     in interp (if isTrue p' then t else f) env

interpApp :: E -> [E] -> Env -> Runtime
interpApp f es env = 
  let (Closure vars body env') = ensureClosure $ interp f env
      runtimes = fmap (flip interp env) es
      newEnv   = Map.union (Map.fromList $ zip vars runtimes) env
  in interp body newEnv
  where
   ensureClosure :: Runtime -> Runtime
   ensureClosure c@(Closure _ _ _) = c
   ensureClosure r = error $ "expected Closure, but got " ++ show r

interpPrim :: Prim -> E -> E -> Env -> Runtime
interpPrim Add      = mathOp (+)
interpPrim Sub      = mathOp (-)
interpPrim Mult     = mathOp (*)
interpPrim LessThan = boolOp (<)
interpPrim LTorEQ   = boolOp (<=)
interpPrim EqualTo  = boolOp (==)

mathOp :: (Int -> Int -> Int) -> E -> E -> Env -> Runtime
mathOp f e1 e2 env = primOp (\x y -> Num $ f x y) e1 e2 env

boolOp :: (Int -> Int -> Bool) -> E -> E -> Env -> Runtime
boolOp f e1 e2 env = primOp (\x y -> Num $ if f x y then 1 else 0) e1 e2 env where

primOp :: (Int -> Int -> Runtime) -> E -> E -> Env -> Runtime
primOp f e1 e2 env = 
  let (Num e1') = ensureNumber $ interp e1 env
      (Num e2') = ensureNumber $ interp e2 env
  in f e1' e2'
  where 
   ensureNumber :: Runtime -> Runtime
   ensureNumber n@(Num _) = n
   ensureNumber _ = error "expected Num, but got a Closure"

