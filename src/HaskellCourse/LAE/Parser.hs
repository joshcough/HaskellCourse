module HaskellCourse.LAE.Parser (parseExp) where

import Data.Maybe (fromMaybe)
import HaskellCourse.Parsing
import HaskellCourse.Prim
import HaskellCourse.LAE.AST

-- | Parse the given s-expression into an LAE 'Exp'.
parseExp :: SExpr -> Exp
parseExp (AtomNum n) = LitInt  n
parseExp (AtomSym s) = parseVar s
parseExp (List [AtomSym "let", List [AtomSym v, e], body]) =
  Let v (parseExp e) (parseExp body)
parseExp (List [AtomSym p, a, b]) = 
  PrimApp (parsePrim p) (parseExp a) (parseExp b)
parseExp bad = error $ "bad expression: " ++ show bad

-- | Attempt to parse a 'Prim' from the given String.
-- | A Var is returned if the String is not a Prim.
parseVar :: String -> Exp
parseVar s = if isPrim s then error ("bad variable name: " ++ s) else Var s
