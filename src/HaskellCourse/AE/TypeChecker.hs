module HaskellCourse.AE.TypeChecker where

import HaskellCourse.AE.AST

data Type = NumT | BoolT | ArrowT Type Type | Poly deriving (Eq, Show)

-- only programs that result in Bool or Num are runnable in this language
typeChecks :: Exp -> Bool
typeChecks e = elem (checkExp e) [BoolT, NumT]

checkExp :: Exp -> Type
checkExp (LitInt  i) = NumT
checkExp (LitBool b) = BoolT
checkExp (PrimExp p) = error $ "unapplied function: " ++ show p
checkExp (App (PrimExp p) args) = checkFunCall (primType p) (fmap checkExp args)

checkFunCall :: Type -> [Type] -> Type
checkFunCall Poly ([lt,rt])      = if lt == rt then BoolT else typeMismatch lt rt
checkFunCall Poly bad            = error $ "== applied to invalid arguments: " ++ show bad
checkFunCall (ArrowT i o) (t:[]) = if i == t then o else typeMismatch i t
checkFunCall (ArrowT i o) (t:ts) = if i == t then checkFunCall o ts else typeMismatch i t
checkFunCall (ArrowT i o) []     = error "function not applied to any arguments"
checkFunCall BoolT _             = error $ "attempt to apply arguments to BoolT"
checkFunCall NumT  _             = error $ "attempt to apply arguments to NumT"

primType :: Prim -> Type
primType Not     = ArrowT BoolT BoolT
primType EqualTo = Poly
primType _       = ArrowT NumT (ArrowT NumT  NumT) 

typeMismatch :: Type -> Type -> a
typeMismatch t1 t2 = error $ "type mismatch: " ++ show t1 ++ ", " ++ show t2
