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
(<= l r)    = [[l]] <= [[r]]
(== l r)    = todo: learn how to represent polymorphism in D.S.
-}

data Runtime = NumR Int | BoolR Bool deriving (Show)

interp :: Exp -> Runtime
interp (LitInt  i)  = NumR i
interp (LitBool b)  = BoolR b
interp (App p e es) = interpPrim p $ fmap interp (e : es)

interpPrim :: Prim -> [Runtime] -> Runtime
interpPrim Add     [NumR  l, NumR  r] = NumR  $ l + r
interpPrim Sub     [NumR  l, NumR  r] = NumR  $ l - r
interpPrim Mult    [NumR  l, NumR  r] = NumR  $ l * r
interpPrim LTorEQ  [NumR  l, NumR  r] = BoolR $ l <= r
interpPrim EqualTo [NumR  l, NumR  r] = BoolR $ l == r
interpPrim EqualTo [BoolR l, BoolR r] = BoolR $ l == r
interpPrim Not     [BoolR b]          = BoolR $ not b

