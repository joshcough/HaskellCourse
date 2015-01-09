module HaskellCourse.AE.Parser (parseExp) where

import Data.Maybe (fromMaybe)
import HaskellCourse.Parsing
import HaskellCourse.Prim
import HaskellCourse.AE.AST

-- | Parse the given s-expression into an AE 'Exp'.
parseExp :: SExpr -> Exp
parseExp (AtomNum  n) = LitInt  n
parseExp (List [AtomSym p, a, b]) = App (parsePrim p) (parseExp a) (parseExp b)
parseExp bad = error $ "parse error, bad expression: " ++ show bad
