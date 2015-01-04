-- |
-- AE stands for Arithmetic Expression, and is a very simple \'language\'.
--
-- AE grammar:
--
-- @
--
--  e    := int | bool | prim | (e e) | (e e e)
--
--  prim := + | - | * | < | == | !
--
-- @
module HaskellCourse.AE.AST where

import HaskellCourse.Util

-- | Corresponds to e in the AE grammar.
data Exp = LitInt Int | LitBool Bool | PrimExp Prim | App Exp Exp

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
  show (PrimExp p) = show p
  show (App f (App a b)) = list [show f, show a, show b]
  show (App f arg) = list [show f, show arg]

unaryApp :: Exp -> Exp -> Exp
unaryApp f a = App f a

binaryApp :: Exp -> Exp -> Exp -> Exp
binaryApp f a b = App (App f a) b 
