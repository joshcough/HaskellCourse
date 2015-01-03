module HaskellCourse.AE.Interp (interp) where

import HaskellCourse.AE.AST

data Runtime = NumR Int | BoolR Bool deriving (Show)

interp :: Exp -> Runtime
interp (LitInt  i)  = NumR i
interp (LitBool b)  = BoolR b
interp (App (PrimExp p) args) = interpPrim p $ fmap interp args

interpPrim :: Prim -> [Runtime] -> Runtime
interpPrim Add     [NumR  l, NumR  r] = NumR  $ l + r
interpPrim Sub     [NumR  l, NumR  r] = NumR  $ l - r
interpPrim Mult    [NumR  l, NumR  r] = NumR  $ l * r
interpPrim LTorEQ  [NumR  l, NumR  r] = BoolR $ l <= r
interpPrim EqualTo [NumR  l, NumR  r] = BoolR $ l == r
interpPrim EqualTo [BoolR l, BoolR r] = BoolR $ l == r
interpPrim Not     [BoolR b]          = BoolR $ not b
