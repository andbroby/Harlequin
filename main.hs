module Main where

import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Char
import Data.Complex
import Data.Ratio

main = do
  args <- getArgs
  putStrLn . readExpr $ head args

data LispVal = Atom String
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
            <|> parseNumber
            <|> parseBool
            <|> parseChar

parseList :: Parser LispVal
parseList = try $ liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = try $ do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = try $ do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

symbol :: Parser Char
symbol = oneOf "!%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match" ++ show err
  Right _ -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

parseComplex :: Parser LispVal
parseComplex = try $ do
  a <- parseFloat <|> parseDec2 <|> parseDec
  char '+'
  b <- parseFloat <|> parseDec2 <|> parseDec
  char 'j'
  return $ Complex ((toDouble a) :+ (toDouble b))
    where toDouble (Float a) = a
          toDouble (Number a) = fromIntegral a

parseRational :: Parser LispVal
parseRational = try $ do
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
parseDec2 = try $ do
  string "#d"
  x <- many1 digit
  return $ Number . read $ x

parseHex :: Parser LispVal
parseHex = try $ do
  string "#x"
  x <- many1 hexDigit
  return $ Number . fst . head . readHex $ x

parseBin :: Parser LispVal
parseBin = try $ do
  string "#b"
  x <- many1 . oneOf $ "10"
  let readBin = foldl (\acc y -> acc*2 + (fromIntegral . ord $ y)) 0
  return $ Number .  readBin $ x

parseBool :: Parser LispVal
parseBool = try $ char '#' >> ((char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False)))

parseOct :: Parser LispVal
parseOct = try $ do
  string "#o"
  x <- many1 octDigit
  return $ Number . fst . head . readOct $ x

parseChar :: Parser LispVal
parseChar = try $ do
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
parseFloat = try $ do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float . fst . head . readFloat $ x ++ "." ++ y

