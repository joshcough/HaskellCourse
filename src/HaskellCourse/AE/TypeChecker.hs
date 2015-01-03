module HaskellCourse.AE.TypeChecker where

import HaskellCourse.AE.AST

{-
Type rules:

<any int> :: IntT, True :: BoolT, False BoolT

    e: BoolT
----------------
(not e) : BoolT

Four rules (written as one) for these primitives: +,-,*,<=

   e1 : IntT, e2: IntT
-------------------------
((+|-|*|<=) e1 e1) : IntT

Two rules for polymorphic equality function:

e1 : IntT, e2: IntT    e1 : BoolT, e2: BoolT
-------------------,   ---------------------
(== e1 e2) : BoolT      (== e1 e2) : BoolT
-}

data Type = NumT | BoolT | ArrowT Type Type | Poly deriving (Eq, Show)

typeCheck :: Exp -> Type
typeCheck (LitInt  _) = NumT
typeCheck (LitBool _) = BoolT
typeCheck (App (PrimExp p) args) =
  let funType  = primType p; argTypes = fmap typeCheck args
  in foldl apply funType argTypes

primType :: Prim -> Type
primType Not     = ArrowT BoolT BoolT
primType EqualTo = Poly
primType _       = ArrowT NumT (ArrowT NumT  NumT) 

apply :: Type -> Type -> Type
apply BoolT  _ = error $ "type error: attempt to apply arguments to BoolT"
apply NumT   _ = error $ "type error: attempt to apply arguments to NumT"
apply (ArrowT i o) t = if t == i then o else typeMismatch i t
apply Poly t = ArrowT t BoolT

typeMismatch :: Type -> Type -> a
typeMismatch t1 t2 = error $ "type mismatch: " ++ show t1 ++ ", " ++ show t2
