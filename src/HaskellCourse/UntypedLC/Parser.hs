module HaskellCourse.UntypedLC.Parser (parseExp) where

import Data.Maybe (fromMaybe)
import HaskellCourse.Parsing
import HaskellCourse.Prim
import HaskellCourse.UntypedLC.AST

parseExp :: SExpr -> Exp
parseExp (AtomNum n) = LitInt  n
parseExp (AtomSym s) = parseVar s
parseExp (List [AtomSym "let", List [AtomSym v, e], body]) =
  Let v (parseExp e) (parseExp body)
parseExp (List [AtomSym v, AtomSym "->", b]) = Lambda v (parseExp b)
parseExp (List [AtomSym p, a, b]) = PrimApp (parsePrim p) (parseExp a) (parseExp b)
parseExp (List [a, b]) = App (parseExp a) (parseExp b)
parseExp bad = error $ "parse error, bad expression: " ++ show bad

parseVar :: String -> Exp
parseVar s = if isPrim s then error ("bad variable name: " ++ s) else Var s
