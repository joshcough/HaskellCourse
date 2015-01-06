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
module HaskellCourse.LAE.TypeChecker (Type(..), typeCheck) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import HaskellCourse.LAE.AST

-- | The Type datatype. 
data Type = IntT | BoolT | ArrowT Type Type | Poly deriving (Eq, Show)

type TypeContext = Map Var Type

-- | Type check the given expression.
typeCheck :: Exp -> TypeContext -> Type
typeCheck (LitInt  _) _ = IntT
typeCheck (LitBool _) _ = BoolT
typeCheck (Var v)     c = fromMaybe (error $ "unknown var: " ++ v) (Map.lookup v c)
typeCheck (Let v e b) c = let te = typeCheck e c in typeCheck b (Map.insert v te c) 
typeCheck (PrimExp p) _ = primType p
typeCheck (App f arg) c = appType (typeCheck f c) (typeCheck arg c)

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
