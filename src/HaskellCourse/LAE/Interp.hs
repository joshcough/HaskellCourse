-- | Denotational semantics of LAE:
--
-- @
-- [[any int]] = itself
-- [[True]]    = true
-- [[False]]   = false
-- (! b)       = not [[b]]
-- (+ l r)     = [[l]] + [[r]]
-- (- l r)     = [[l]] - [[r]]
-- (* l r)     = [[l]] * [[r]]
-- (< l r)     = [[l]] < [[r]]
-- (== l r)    = todo: learn how to represent polymorphism in D.S.
-- @
module HaskellCourse.LAE.Interp (interp, Runtime(..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import HaskellCourse.LAE.AST

data Runtime = IntR Int | BoolR Bool deriving (Show)

type Env = Map Var Runtime

-- | Interpret the given Expression, producing a Runtime value.
interp :: Exp -> Env -> Runtime
interp (LitInt  i) _   = IntR i
interp (LitBool b) _   = BoolR b
interp (Var     v) env = fromMaybe (error $ "unknown var: " ++ v) (Map.lookup v env)
interp (Let v e b) env = let re = interp e env in interp b (Map.insert v re env) 
interp (App (PrimExp Not) a) env = case interp a env of BoolR b -> BoolR (not b)
interp (App (App (PrimExp p) a) b) env = interpBinaryApp p (interp a env) (interp b env)

-- | Apply a binary function to its arguments.
interpBinaryApp :: Prim -> Runtime -> Runtime -> Runtime
interpBinaryApp Add      (IntR  l) (IntR  r) = IntR  $ l + r
interpBinaryApp Sub      (IntR  l) (IntR  r) = IntR  $ l - r
interpBinaryApp Mult     (IntR  l) (IntR  r) = IntR  $ l * r
interpBinaryApp LessThan (IntR  l) (IntR  r) = BoolR $ l < r
interpBinaryApp EqualTo  (IntR  l) (IntR  r) = BoolR $ l == r
interpBinaryApp EqualTo  (BoolR l) (BoolR r) = BoolR $ l == r
