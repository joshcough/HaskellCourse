-- |
-- The L in LAE stands for Let. LAE is AE with let statements.
--
-- LAE grammar:
--
-- @
--
--  e    := int | bool | prim | (e e) | (e e e) | 
--          (let (var e) e) | var
--
--  prim := + | - | * | < | == | !
--
--  var  := [a-z]+
--
-- @
module HaskellCourse.LAE.AST where

import HaskellCourse.Util

type Var = String

-- | Corresponds to e in the AE grammar.
data Exp = LitInt Int | LitBool Bool | PrimExp Prim | App Exp Exp |
           Let Var Exp Exp | Var Var

-- | Corresponds to prim in the AE grammar.
data Prim = Add | Sub | Mult | LessThan | EqualTo | Not 

instance Show Prim where
  show Add      = "+"
  show Sub      = "-"
  show Mult     = "*"
  show LessThan = "<="
  show EqualTo  = "=="
  show Not      = "not"

instance Show Exp where
  show (LitInt  i) = show i
  show (LitBool b) = show b
  show (Var     v) = show v
  show (PrimExp p) = show p
  show (App f (App a b)) = list [show f, show a, show b]
  show (App f arg) = list [show f, show arg]
  show (Let v e b) = list ["let", list [v, show e], show b]

unaryApp :: Exp -> Exp -> Exp
unaryApp f a = App f a

binaryApp :: Exp -> Exp -> Exp -> Exp
binaryApp f a b = App (App f a) b 
