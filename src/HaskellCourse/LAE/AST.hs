-- |
-- The L in LAE stands for Let. LAE is AE with let statements.
--
-- LAE grammar:
--
-- @
--
--  e    := int | (prim e e) | (let (var e) e) | var
--
--  prim := + | - | * | /
--
--  var  := [a-z]+
--
-- @
module HaskellCourse.LAE.AST where

import HaskellCourse.Prim
import HaskellCourse.Util

type Var = String

data Exp = LitInt Int      | PrimApp Prim Exp Exp | 
           Let Var Exp Exp | Var Var

instance Show Exp where
  show (LitInt  i)     = show i
  show (Var     v)     = show v
  show (PrimApp p a b) = list [show p, show a, show b]
  show (Let v e b)     = list ["let", list [v, show e], show b]
