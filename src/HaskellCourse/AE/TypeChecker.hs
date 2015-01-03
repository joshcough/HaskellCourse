module HaskellCourse.AE.TypeChecker where

import HaskellCourse.AE.AST

data Type = NumT | BoolT | ArrowT Type Type | TyVar deriving (Eq, Show)

tc :: Exp -> Type
tc (LitInt  i)  = NumT
tc (LitBool b)  = BoolT
tc (PrimExp p)  = primType p
tc (App f args) = checkFunCall (tc f) (fmap tc args)

checkFunCall :: Type -> [Type] -> Type
checkFunCall funT ([lt,rt]) | funT == polyFun = if lt == rt then BoolT else typeMismatch lt rt
checkFunCall (ArrowT i o) []     = error "function not applied to any arguments"
checkFunCall (ArrowT i o) (t:[]) = if i == t then o else typeMismatch i t
checkFunCall (ArrowT i o) (t:ts) = if i == t then checkFunCall o ts else typeMismatch i t
checkFunCall badT _ = error $ "attempt to apply arguments to: " ++ show badT

-- Num  -> Num  -> Num
numFun  = ArrowT NumT  (ArrowT NumT  NumT)
-- a    -> a    -> a
polyFun = ArrowT TyVar (ArrowT TyVar TyVar)

primType :: Prim -> Type
primType Add     = numFun
primType Sub     = numFun
primType Mult    = numFun
primType LTorEQ  = numFun
primType Not     = ArrowT BoolT BoolT -- Bool -> Bool
primType EqualTo = polyFun

typeMismatch :: Type -> Type -> a
typeMismatch t1 t2 = error $ "type mismatch: " ++ show t1 ++ ", " ++ show t2
