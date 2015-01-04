module HaskellCourse.AE.Parser (parseExp) where

import Data.Maybe (fromMaybe)
import HaskellCourse.Parsing
import HaskellCourse.AE.AST

-- | Parse the given s-expression into an AE 'Exp'.
parseExp :: SExpr -> Exp
parseExp (AtomNum  n)     = LitInt  n
parseExp (AtomBool b)     = LitBool b
parseExp (AtomSym  s)     = PrimExp $ parsePrim s  
parseExp (List [f, a])    = unaryApp  (parseExp f) (parseExp a) 
parseExp (List [f, a, b]) = binaryApp (parseExp f) (parseExp a) (parseExp b)
parseExp bad = error $ "parse error, bad expression: " ++ show bad

-- | Maps the textual representation of built in functions
-- | to the 'Prim' object that backs them.
prims = [("+", Add), ("-", Sub), ("*", Mult), 
         ("<", LessThan), ("==", EqualTo), ("!", Not)]

-- | Attempt to parse a 'Prim' from the given String.
-- | An error is raise if the given String is unknown.
parsePrim :: String -> Prim
parsePrim s = fromMaybe 
                (error $ "parse error, unknown prim: " ++ s) 
                (lookup s prims)
