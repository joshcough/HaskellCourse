{-# LANGUAGE TemplateHaskell #-}
module Properties where

import Control.Applicative
import HaskellCourse.AE.Main
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck

prop_all_exps_typecheck :: Exp -> Bool
prop_all_exps_typecheck e = typeCheck e `elem` [BoolT, NumT]

-- arbitrary expressions resulting in Int or Bool
instance Arbitrary Exp where
  arbitrary = oneof [intExp, boolExp]

-- generate expressions that return ints
intExp  = oneof [ LitInt <$> arbitrary, 
                  App    <$> intPrim <*> intExp <*> single intExp ] where
  intPrim = oneof $ return <$> [ Add, Sub, Mult ]

-- expressions that return bools
boolExp = oneof [ LitBool <$> arbitrary, 
                  App <$> return Not      <*> boolExp <*> return [],
                  App <$> return LessThan <*> intExp  <*> single intExp,
                  App <$> return EqualTo  <*> boolExp <*> single boolExp,
                  App <$> return EqualTo  <*> intExp  <*> single intExp ]

single g = (:[]) <$> g

tests = $testGroupGenerator
