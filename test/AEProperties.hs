{-# LANGUAGE TemplateHaskell #-}
module AEProperties where

import Control.Applicative
import HaskellCourse.AE.AE
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck

-- | Ensure all Exps typecheck and are runnable
prop_all_exps_typecheck :: Exp -> Bool
prop_all_exps_typecheck e = typeCheck e `elem` [BoolT, IntT]

-- | Ensure that all Exps of type IntT or BoolT run, 
-- and produce a value of the correct type.
prop_all_exps_typecheck_and_run :: Exp -> Bool
prop_all_exps_typecheck_and_run e = 
  typeOf (interp e) == typeCheck e where
    typeOf (BoolR _) = BoolT
    typeOf (IntR  _) = IntT

-- | Arbitrary expressions resulting in values of type IntT or BoolT
instance Arbitrary Exp where arbitrary = oneof [intExp, boolExp]

-- | Arbitrary expressions resulting in values of type IntT
intExp = oneof [ LitInt    <$> arbitrary, 
                 binaryApp <$> intPrim <*> intExp <*> intExp ] where
  intPrim = oneof $ return . PrimExp <$> [ Add, Sub, Mult ]

-- | Arbitrary expressions resulting in values of type BoolT
boolExp = oneof [ LitBool    <$> arbitrary, 
                  unaryApp   <$> return (PrimExp Not)      <*> boolExp,
                  binaryApp  <$> return (PrimExp LessThan) <*> intExp  <*> intExp,
                  binaryApp  <$> return (PrimExp EqualTo)  <*> boolExp <*> boolExp,
                  binaryApp  <$> return (PrimExp EqualTo)  <*> intExp  <*> intExp ]

-- | This has to be here to run quickcheck tests.
tests = $testGroupGenerator
