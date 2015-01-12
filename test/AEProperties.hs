{-# LANGUAGE TemplateHaskell #-}
module AEProperties where

import Control.Applicative
import HaskellCourse.AE.AE
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck

-- | Ensure that all Exps run, and produce an int.
all_exps_typecheck_and_run :: Exp -> Bool
all_exps_typecheck_and_run e =
  let i = interp e in i <= 0 || i > 0

-- | Arbitrary expressions resulting in values of type IntT or BoolT
instance Arbitrary Exp where 
  arbitrary = arbExp where
    arbExp = oneof [ LitInt <$> arbitrary, 
                     App    <$> prim <*> arbExp <*> arbExp ] where
    prim = oneof $ return <$> [ Add, Mult ]

-- | This has to be here to run quickcheck tests.
tests = $testGroupGenerator

