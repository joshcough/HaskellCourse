module HaskellCourse.LAE.Parser (parseExp) where

import Data.Maybe (fromMaybe)
import HaskellCourse.Parsing
import HaskellCourse.LAE.AST

-- | Parse the given s-expression into an LAE 'Exp'.
parseExp :: SExpr -> Exp
parseExp (AtomNum  n)     = LitInt  n
parseExp (AtomBool b)     = LitBool b
parseExp (AtomSym  s)     = parsePrimOrVar s  
parseExp (List [f, a])    = unaryApp  (parseExp f) (parseExp a)
-- (let (var e) e) 
parseExp (List [AtomSym  "let", List [AtomSym v, e], body]) =
  Let v (parseExp e) (parseExp body)
parseExp (List [f, a, b]) = binaryApp (parseExp f) (parseExp a) (parseExp b)
parseExp bad = error $ "parse error, bad expression: " ++ show bad

-- | Maps the textual representation of built in functions
-- | to the 'Prim' object that backs them.
prims = [("+", Add), ("-", Sub), ("*", Mult), 
         ("<", LessThan), ("==", EqualTo), ("!", Not)]

-- | Attempt to parse a 'Prim' from the given String.
-- | A Var is returned if the String is not a Prim.
parsePrimOrVar :: String -> Exp
parsePrimOrVar s = maybe (Var s) PrimExp (lookup s prims)
