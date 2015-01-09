module HaskellCourse.Prim where

import Data.Maybe (fromMaybe)

data Prim = Add | Sub | Mult | Div

instance Show Prim where
  show Add  = "+"
  show Sub  = "-"
  show Mult = "*"
  show Div  = "/"

-- | Maps the textual representation of built in functions
-- | to the 'Prim' object that backs them.
prims = [("+", Add), ("-", Sub), ("*", Mult), ("/", Div)]

-- | Attempt to parse a 'Prim' from the given String.
-- | An error is raise if the given String is unknown.
parsePrim :: String -> Prim
parsePrim s = fromMaybe (error $ "unknown prim: " ++ s) (lookup s prims)

isPrim :: String -> Bool
isPrim s = maybe False (\_ -> True) (lookup s prims)

runPrim :: Prim -> Int -> Int -> Int
runPrim Add  l r = l + r
runPrim Sub  l r = l - r
runPrim Mult l r = l * r
runPrim Div  l r = l `div` r