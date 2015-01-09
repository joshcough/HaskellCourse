-- |
-- AE stands for Arithmetic Expression, and is a very simple \'language\'.
--
-- AE grammar:
--
-- @
--
--  e    := int | (prim e e)
--
--  prim := + | - | * | /
--
-- @
module HaskellCourse.AE.AST where

import HaskellCourse.Prim
import HaskellCourse.Util

-- | Corresponds to e in the AE grammar.
data Exp = LitInt Int | App Prim Exp Exp

instance Show Exp where
  show (LitInt  i) = show i
  show (App p a b) = list [show p, show a, show b]
