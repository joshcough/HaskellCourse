module HaskellCourse.AE.Parser where

import Data.Maybe (fromMaybe)
import HaskellCourse.Parsing
import HaskellCourse.AE.AST

parseString :: String -> Exp
parseString = parseExp . readSExpr

parseExp :: SExpr -> Exp
parseExp (AtomNum  n)   = LitInt  n
parseExp (AtomBool b)   = LitBool b
parseExp (List ((AtomSym f) : e : es)) = 
  App (parsePrim f) (parseExp e) (fmap parseExp es)
parseExp bad = error $ "parse error, bad expression: " ++ show bad

prims = [("+", Add), ("-", Sub), ("*", Mult), 
         ("<=", LTorEQ), ("==", EqualTo), ("!", Not)]

parsePrim :: String -> Prim
parsePrim s = fromMaybe 
                (error $ "parse error, unknown prim: " ++ s) 
                (lookup s prims)
