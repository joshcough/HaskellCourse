module HaskellCourse.AE.Parser where

import Data.Maybe (fromMaybe)
import HaskellCourse.Parsing
import HaskellCourse.AE.AST

parseExp :: SExpr -> Exp
parseExp (AtomNum  n)     = LitInt  n
parseExp (AtomBool b)     = LitBool b
parseExp (AtomSym  s)     = PrimExp $ parsePrim s  
parseExp (List [f, a])    = App (parseExp f) (parseExp a) 
parseExp (List [f, a, b]) = App (App (parseExp f) (parseExp a)) (parseExp b)
parseExp bad = error $ "parse error, bad expression: " ++ show bad

prims = [("+", Add), ("-", Sub), ("*", Mult), 
         ("<", LessThan), ("==", EqualTo), ("!", Not)]

parsePrim :: String -> Prim
parsePrim s = fromMaybe 
                (error $ "parse error, unknown prim: " ++ s) 
                (lookup s prims)
