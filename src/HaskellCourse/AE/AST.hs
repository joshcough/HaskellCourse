module HaskellCourse.AE.AST where

import HaskellCourse.Util

type Variable = String

data Prim = Add | Sub | Mult | LTorEQ | EqualTo | Not

data Exp = LitInt Int | LitBool Bool | PrimExp Prim | App Exp [Exp]

instance Show Prim where
  show Add      = "+"
  show Sub      = "-"
  show Mult     = "*"
  show LTorEQ   = "<="
  show EqualTo  = "=="
  show Not      = "not"

instance Show Exp where
  show (LitInt  i) = show i
  show (LitBool b) = show b
  show (PrimExp p) = show p
  show (App p es)  = list $ show p : fmap show es

