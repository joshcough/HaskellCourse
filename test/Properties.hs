{-# LANGUAGE TemplateHaskell #-}
module Properties where

import Control.Applicative
import Debug.Trace
import HaskellCourse.AE.Main
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck

prop_all_exps_typecheck :: Exp -> Bool
prop_all_exps_typecheck e = typeCheck e `elem` [BoolT, IntT]

prop_all_exps_typecheck_and_run :: Exp -> Bool
prop_all_exps_typecheck_and_run e = trace msg $ typeOf r == t where
  t   = typeCheck e
  r   = interp e
  msg = "exp: " ++ show e ++ ": " ++ show t ++ " ==> " ++ show r
  typeOf (BoolR _) = BoolT
  typeOf (IntR  _) = IntT

-- arbitrary expressions resulting in Int or Bool
instance Arbitrary Exp where arbitrary = oneof [intExp, boolExp]

-- generate expressions that return ints
intExp = oneof [ LitInt    <$> arbitrary, 
                 binaryApp <$> intPrim <*> intExp <*> intExp ] where
  intPrim = oneof $ return . PrimExp <$> [ Add, Sub, Mult ]

-- expressions that return bools
boolExp = oneof [ LitBool    <$> arbitrary, 
                  unaryApp   <$> return (PrimExp Not)      <*> boolExp,
                  binaryApp  <$> return (PrimExp LessThan) <*> intExp  <*> intExp,
                  binaryApp  <$> return (PrimExp EqualTo)  <*> boolExp <*> boolExp,
                  binaryApp  <$> return (PrimExp EqualTo)  <*> intExp  <*> intExp ]

tests = $testGroupGenerator
