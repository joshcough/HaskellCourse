module HaskellCourse.AE.AST where

import HaskellCourse.Util

{-
Grammar:

e    := int | bool | (prim e ...)
prim := +|-|*|<=|==|!
-}

data Prim = Add | Sub | Mult | LTorEQ | EqualTo | Not 
  deriving Show

data Exp = LitInt Int | LitBool Bool | App Prim Exp [Exp]
  deriving Show
