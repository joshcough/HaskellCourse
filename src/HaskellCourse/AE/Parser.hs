module HaskellCourse.AE.Parser where

import HaskellCourse.Parsing
import HaskellCourse.AE.AST

parseString :: String -> Exp
parseString = parseExp . readSExpr

parseExp :: SExpr -> Exp
parseExp (AtomNum  n)    = LitInt  n
parseExp (AtomBool b)    = LitBool b
parseExp (AtomSym  fun)  = parseFun fun
parseExp (List (e : es)) = App (parseExp e) (fmap parseExp es)

parseFun :: String -> Exp
parseFun "+"   = PrimExp Add
parseFun "-"   = PrimExp Sub
parseFun "*"   = PrimExp Mult
parseFun "<="  = PrimExp LTorEQ
parseFun "=="  = PrimExp EqualTo
parseFun "not" = PrimExp Not
parseFun bad   = error $ "unknown function: " ++ bad
