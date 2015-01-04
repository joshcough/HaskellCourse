module Main where

import AEProperties
import UnitTests
import Test.Framework.Runners.Console (defaultMain)

main = defaultMain $ [UnitTests.tests, AEProperties.tests]
