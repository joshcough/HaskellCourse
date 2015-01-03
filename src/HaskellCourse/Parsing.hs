module HaskellCourse.Parsing 
  (
    ParseResult
   ,SExpr(..)
   ,flatten
   ,liftParser
   ,runParser
   ,runParserOrDie
   ,intParser
   ,sexprParser
   ,readSExprWithRest
   ,readSExpr
   ,trim
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString.UTF8 as UTF8 hiding (lines)
import Data.Char (isDigit, isSpace)
import qualified Data.HashSet as HashSet
import Data.Semigroup
import Text.Parser.Combinators
import Text.Parser.Token
import qualified Text.Parser.Token.Highlight as Highlight
import Text.Read (readMaybe)
import Text.Trifecta hiding (semi)

import HaskellCourse.Util

type ParseResult a = Either String a

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

data SExpr = 
    AtomSym String 
  | AtomNum Int 
  | AtomBool Bool 
  | List [SExpr] deriving (Eq)

instance Show SExpr where
  show (AtomSym  s) = s
  show (AtomNum  i) = show i
  show (AtomBool b) = show b
  show (List exps)  = list $ fmap show exps

readSExpr :: String -> SExpr
readSExpr = runParserOrDie sexprParser . preprocess

readSExprWithRest :: String -> (SExpr, String)
readSExprWithRest = runParserOrDie ((,) <$> sexprParser <*> many anyChar) . preprocess

preprocess :: String -> String
preprocess = trim . concat . map f . lines where
  f = (++ " ") . trim . removeComments
  removeComments = takeWhile $ not . (==';')

flatten :: SExpr -> [String]
flatten (AtomSym s)  = [s]
flatten (AtomNum n)  = [show n]
flatten (AtomBool b) = [show b]
flatten (List ss)    = ss >>= flatten

liftParser :: (SExpr -> ParseResult p) -> String -> ParseResult p
liftParser f = f . readSExpr

sexprParser :: Parser SExpr
sexprParser = choice [intParser, boolParser, symParser, listParser] 

variable :: TokenParsing m => IdentifierStyle m
variable = IdentifierStyle
  { _styleName = "token"
  , _styleStart = varInit
  , _styleLetter = varSubsequent
  , _styleReserved = HashSet.empty
  , _styleHighlight = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
} where
  varInit = digit <|> letter <|> oneOf "!$%&*/:<=>?~_^-+"
  varSubsequent = varInit <|> oneOf ".+="

tokenParser :: (String -> Maybe r) -> Parser r
tokenParser f = try $ do 
  x <- ident variable
  maybe mzero return $ f x

intParser :: Parser SExpr
intParser = tokenParser (fmap AtomNum . readMaybe) <?> "int"

boolParser :: Parser SExpr
boolParser = tokenParser (fmap AtomBool . readMaybe) <?> "bool"

symParser :: Parser SExpr
symParser = tokenParser sym <?> "symbol" where
  sym (x:xs) | not (isDigit x) = Just $ AtomSym (x:xs)
  sym _ = Nothing

listParser :: Parser SExpr
listParser = List <$> (parens recur <|> brackets recur) where
  recur = many sexprParser

runParser :: Parser a -> String -> ParseResult a
runParser p s = case parseByteString p mempty (UTF8.fromString s) of
  Failure xs -> error (show xs) -- Left $ show xs
  Success a -> Right a

runParserOrDie :: Parser a -> String -> a
runParserOrDie p s = either error id (runParser p s)

