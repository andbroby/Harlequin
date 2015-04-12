module Main where

import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Char
import Data.Complex
import Data.Ratio
import qualified Data.Array as A

main = getArgs >>= print . eval . readExpr . head

instance Show LispVal where
  show (String content) = "\"" ++ content ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Atom name) = name
  show (Number content) = show content
  show (List content) = "(" ++ unwordList content ++ ")"
  show (DottedList head tail) = "(" ++ unwordList head ++ " . " ++ show tail ++ ")"

unwordList :: [LispVal] -> String
unwordList = unwords . map show

data LispVal = Atom String
             | Nil ()
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Complex (Complex Double)
             | Int Integer
             | Ratio Rational
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Vector (A.Array Int LispVal)


parseNumber :: Parser LispVal
parseNumber = parseDec
              <|> parseDec2
              <|> parseHex
              <|> parseBin
              <|> parseOct
              <|> parseFloat

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> try parseNumber
            <|> try parseBool
            <|> try parseChar
            <|> parseQuoted
            <|> parseQuasiQuote
            <|> parseQuasiUnquote
            <|> parseVector
            <|> parseList

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func:args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("*", numericBinOp (*)),
              ("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("/", numericBinOp div),
              ("mod", numericBinOp mod),
              ("quotient", numericBinOp quot),
              ("remainder", numericBinOp rem),
              ("symbol?", unaryOp symbolq),
              ("string?", unaryOp stringq),
              ("number?", unaryOp numberq),
              ("bool?", unaryOp boolq),
              ("list?", unaryOp listq)]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [val] = f val

symbolq, stringq, numberq, boolq, listq :: LispVal -> LispVal
symbolq (Atom _) = Bool True
symbolq _ = Bool False
stringq (String _) = Bool True
stringq _ = Bool False
boolq (Bool _) = Bool True
boolq _ = Bool False
listq (List _) = Bool True
listq (DottedList _ _) = Bool True
listq _ = Bool False
numberq (Number _) = Bool True
numberq _ = Bool False


numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match" ++ show err
  Right value -> value

parseVector :: Parser LispVal
parseVector = try $ do
  string "#("
  content <- parseVectorContent
  char ')'
  return content

optionalSpace :: Parser ()
optionalSpace = skipMany space

parseVectorContent :: Parser LispVal
parseVectorContent = do
  arrayValues <- sepBy parseExpr spaces
  return $ Vector (A.listArray (0, (length arrayValues - 1)) arrayValues)

parseList :: Parser LispVal
parseList = do
  char '('
  optionalSpace
  head <- sepEndBy parseExpr spaces
  tail <- (char '.' >> spaces >> parseExpr) <|> return (Nil ())
  optionalSpace
  char ')'
  return $ case tail of
    (Nil ()) -> List head
    otherwise -> DottedList head tail

parseQuasiQuote :: Parser LispVal
parseQuasiQuote = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseQuasiUnquote :: Parser LispVal
parseQuasiUnquote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

symbol :: Parser Char
symbol = oneOf "!%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseComplex :: Parser LispVal
parseComplex = do
  a <- parseFloat <|> parseDec2 <|> parseDec
  char '+'
  b <- parseFloat <|> parseDec2 <|> parseDec
  char 'j'
  return $ Complex ((toDouble a) :+ (toDouble b))
    where toDouble (Float a) = a
          toDouble (Number a) = fromIntegral a

parseRational :: Parser LispVal
parseRational = do
  n <- many1 digit
  char '/'
  d <- many1 digit
  return $ Ratio ((read n) % (read d))

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"" <|> escapedChar)
  char '"'
  return $ String x

escapedChar :: Parser Char
escapedChar = do
  char '\\'
  c <- oneOf "nrt\\\""
  return $ case c of
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _ -> c

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseDec :: Parser LispVal
parseDec = liftM (Number . read) $ many1 digit

parseDec2 :: Parser LispVal
parseDec2 = do
  string "#d"
  x <- many1 digit
  return $ Number . read $ x

parseHex :: Parser LispVal
parseHex = do
  string "#x"
  x <- many1 hexDigit
  return $ Number . fst . head . readHex $ x

parseBin :: Parser LispVal
parseBin = do
  string "#b"
  x <- many1 . oneOf $ "10"
  let readBin = foldl (\acc y -> acc*2 + (fromIntegral . ord $ y)) 0
  return $ Number .  readBin $ x

parseBool :: Parser LispVal
parseBool = char '#' >> ((char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False)))

parseOct :: Parser LispVal
parseOct = do
  string "#o"
  x <- many1 octDigit
  return $ Number . fst . head . readOct $ x

parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  let alphaLower = ['a'..'z']
      alphaUpper = map toUpper alphaLower
      symbols = " (" ++ alphaLower ++ alphaUpper
  x <- string "newline"
       <|> string "space"
       <|> do
         x <- oneOf symbols
         return $ show x
  return $ Character $ case x of
    "space" -> ' '
    "newline" -> '\n'
    otherwise -> head x

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float . fst . head . readFloat $ x ++ "." ++ y

