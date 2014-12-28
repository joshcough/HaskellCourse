module HaskellCourse.Lang1.AST (Variable, Prim(..), E(..)) where

import HaskellCourse.Util

type Variable = String

data Prim = Add | Sub | Mult | LessThan | LTorEQ | EqualTo deriving (Eq,Ord)

data E =
    LitInt Int
  | Var Variable
  | Let Variable E E
  | IfStatement E E E
  | Lambda [Variable] E
  | App E [E]
  | PrimApp Prim E E
  deriving Eq

instance Show E where
  show (LitInt i)          = show i
  show (Var v)             = v
  show (Let v e b)         = list ["let", list [v, show e], show b]
  show (IfStatement p t f) = list ["if", show p, show t, show f]
  show (Lambda vs e)       = list ["\\" ++ spaced vs, "->", show e] 
  show (App e es)          = list $ fmap show (e : es)

