module HaskellCourse.AE.Interp (interp, Runtime(..)) where

import HaskellCourse.AE.AST

{-
Denotational semantics:

[[any int]] = itself
[[True]]    = true
[[False]]   = false
(! b)       = not [[b]]
(+ l r)     = [[l]] + [[r]]
(- l r)     = [[l]] - [[r]]
(* l r)     = [[l]] * [[r]]
(< l r)     = [[l]] < [[r]]
(== l r)    = todo: learn how to represent polymorphism in D.S.
-}

data Runtime = IntR Int | BoolR Bool deriving (Show)

interp :: Exp -> Runtime
interp (LitInt  i)  = IntR i
interp (LitBool b)  = BoolR b
interp (App (PrimExp Not) a) = case interp a of BoolR b -> BoolR (not b)
interp (App (App (PrimExp p) a) b) = interpBinaryApp p (interp a) (interp b)

interpBinaryApp :: Prim -> Runtime -> Runtime -> Runtime
interpBinaryApp Add      (IntR  l) (IntR  r) = IntR  $ l + r
interpBinaryApp Sub      (IntR  l) (IntR  r) = IntR  $ l - r
interpBinaryApp Mult     (IntR  l) (IntR  r) = IntR  $ l * r
interpBinaryApp LessThan (IntR  l) (IntR  r) = BoolR $ l < r
interpBinaryApp EqualTo  (IntR  l) (IntR  r) = BoolR $ l == r
interpBinaryApp EqualTo  (BoolR l) (BoolR r) = BoolR $ l == r
