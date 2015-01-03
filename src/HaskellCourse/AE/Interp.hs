module HaskellCourse.AE.Interp (interp, Runtime) where

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

data Runtime = NumR Int | BoolR Bool deriving (Show)

interp :: Exp -> Runtime
interp (LitInt  i)  = NumR i
interp (LitBool b)  = BoolR b
interp (App (PrimExp Not) a) = case interp a of BoolR b -> BoolR (not b)
interp (App (App (PrimExp p) a) b) = interpBinaryPrim p (interp a) (interp b)

interpBinaryPrim :: Prim -> Runtime -> Runtime -> Runtime
interpBinaryPrim Add      (NumR  l) (NumR  r) = NumR  $ l + r
interpBinaryPrim Sub      (NumR  l) (NumR  r) = NumR  $ l - r
interpBinaryPrim Mult     (NumR  l) (NumR  r) = NumR  $ l * r
interpBinaryPrim LessThan (NumR  l) (NumR  r) = BoolR $ l < r
interpBinaryPrim EqualTo  (NumR  l) (NumR  r) = BoolR $ l == r
interpBinaryPrim EqualTo  (BoolR l) (BoolR r) = BoolR $ l == r

