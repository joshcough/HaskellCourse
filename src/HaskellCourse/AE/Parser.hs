module HaskellCourse.AE.Parser where

import HaskellCourse.Parsing
import HaskellCourse.AE.AST

parseString :: String -> Exp
parseString = parseExp . readSExpr

parseExp :: SExpr -> Exp
parseExp (AtomNum  n)   = LitInt  n
parseExp (AtomBool b)   = LitBool b
parseExp (List ((AtomSym f) : es)) = App (parsePrim f) (fmap parseExp es)
parseExp bad = error $ "parse error, bad expression: " ++ show bad

parsePrim :: String -> Exp
parsePrim "+"   = PrimExp Add
parsePrim "-"   = PrimExp Sub
parsePrim "*"   = PrimExp Mult
parsePrim "<="  = PrimExp LTorEQ
parsePrim "=="  = PrimExp EqualTo
parsePrim "not" = PrimExp Not
parsePrim bad   = error $ "parse error, unknown function: " ++ bad
