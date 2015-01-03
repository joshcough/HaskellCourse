module HaskellCourse.AE.AST where

import HaskellCourse.Util

{-
Grammar:

e    := int | bool | (not e) | (prim e e)
prim := +|-|*|<=|==
-}

data Prim = Add | Sub | Mult | LTorEQ | EqualTo | Not 
  deriving Show

data Exp = LitInt Int | LitBool Bool | PrimExp Prim | App Exp [Exp]
  deriving Show
