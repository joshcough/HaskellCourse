-- |
-- Type rules:
--
-- @
-- (any int) :: IntT, True :: BoolT, False BoolT
-- 
--    e: BoolT            e1 : IntT, e2: IntT
-- --------------      -------------------------
-- (! e) : BoolT           (< e1 e2) : BoolT
-- 
-- Three rules (written as one) for these primitives: +,-,*
-- 
--   e1 : IntT, e2: IntT
-- ------------------------
--  ((+|-|*) e1 e2) : IntT
-- 
-- Two rules for polymorphic equality function:
-- 
-- e1 : IntT, e2: IntT     e1 : BoolT, e2: BoolT
-- -------------------     ---------------------
-- (== e1 e2) : BoolT       (== e1 e2) : BoolT
-- @
module HaskellCourse.AE.TypeChecker (Type(..), typeCheck) where

import HaskellCourse.AE.AST

-- | The Type datatype. 
data Type = IntT | BoolT | ArrowT Type Type | Poly deriving (Eq, Show)

-- | Type check the given expression.
typeCheck :: Exp -> Type
typeCheck (LitInt  _) = IntT
typeCheck (LitBool _) = BoolT
typeCheck (PrimExp p) = primType p
typeCheck (App f arg) = appType (typeCheck f) (typeCheck arg)

-- | Return the type for the given Prim.
primType :: Prim -> Type
primType Not      = ArrowT BoolT BoolT
primType EqualTo  = Poly
primType LessThan = ArrowT IntT (ArrowT IntT BoolT) 
primType _        = ArrowT IntT (ArrowT IntT IntT) 

-- | Apply a function type, returning its output if possible.
appType :: Type -> Type -> Type
appType (ArrowT i o) t = if t == i then o else typeMismatch i t
appType Poly t = ArrowT t BoolT
appType bad  _ = error $ "type error: attempt to apply arguments to: " ++ show bad

-- | Raise a type error.
typeMismatch :: Type -> Type -> a
typeMismatch t1 t2 = error $ "type mismatch: " ++ show t1 ++ ", " ++ show t2
