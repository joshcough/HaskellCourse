module HaskellCourse.Util where

import Data.List

list :: [String] -> String
list as = "(" ++ spaced as ++ ")"

spaced :: [String] -> String
spaced as = concat $ intersperse " " as
