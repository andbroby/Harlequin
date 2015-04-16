module Main where

import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Char
import Data.Complex
import Data.Ratio
import qualified Data.Array as A
import Control.Monad.Error

main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

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

instance Show LispVal where
  show (String content) = "\"" ++ content ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Atom name) = name
  show (Number content) = show content
  show (List content) = "(" ++ unwordList content ++ ")"
  show (DottedList head tail) = "(" ++ unwordList head ++ " . " ++ show tail ++ ")"

instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (NumArgs expected found) = "Expected " ++
                                  show expected ++
                                  " args, found" ++
                                  show found
  show (TypeMismatch expected found) = "Invalid type. Expected " ++
                                       expected ++
                                       " found " ++
                                       show found
  show (Parser parseError) = "Parse error at " ++ show parseError
  show (NotFunction message func) = message ++ ": " ++ show func
  show (BadSpecialForm message form) = message ++ ": " ++ show form

instance Error LispError where
  noMsg = Default "An error occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
    
unwordList :: [LispVal] -> String
unwordList = unwords . map show

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right value -> return value

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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
  do result <- eval pred
     case result of
      Bool False -> eval alt
      otherwise -> eval conseq
eval (List (Atom func:args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form " badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                  ($args)
                  (lookup func primitives)

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [x] = throwError $ TypeMismatch "pair" x
car x = throwError $ NumArgs 1 x

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [x] = throwError $ TypeMismatch "pair" x
cdr x = throwError $ NumArgs 1 x

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List[]] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons x = throwError $ NumArgs 2 x

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
                                 where eqvPair (x1, x2) = case eqv [x1, x2] of
                                                           Left err -> False
                                                           Right (Bool b) -> b
eqv [_, _] = return $ Bool $ False
eqv x = throwError $ NumArgs 2 x

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
              ("list?", unaryOp listq),
              ("=", numBoolBinOp (==)),
              ("<", numBoolBinOp (<)),
              (">", numBoolBinOp (>)),
              ("/=", numBoolBinOp (/=)),
              (">=", numBoolBinOp (>=)),
              ("<=", numBoolBinOp (<=)),
              ("&&", boolBoolBinOp (&&)),
              ("||", boolBoolBinOp (||)),
              ("string=?", strBoolBinOp (==)),
              ("string<?", strBoolBinOp (<)),
              ("string>?", strBoolBinOp (>)),
              ("string<=?", strBoolBinOp (<=)),
              ("string>=?", strBoolBinOp (>=))]

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ last args
                                     return $ Bool $ left `op` right

numBoolBinOp = boolBinOp unpackNum
strBoolBinOp = boolBinOp unpackStr
boolBoolBinOp = boolBinOp unpackBool

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [val] = f val
unaryOp f [] = throwError $ NumArgs 1 []

symbolq, stringq, numberq, boolq, listq :: LispVal -> ThrowsError LispVal
symbolq (Atom _) = Right (Bool True)
symbolq _ = Right (Bool False)
stringq (String _) = Right (Bool True)
stringq _ = Right (Bool False)
boolq (Bool _) = Right (Bool True)
boolq _ = Right (Bool False)
listq (List _) = Right (Bool True)
listq (DottedList _ _) = Right (Bool True)
listq _ = Right (Bool False)
numberq (Number _) = Right (Bool True)
numberq _ = Right (Bool False)

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp op [] = throwError $ NumArgs 2 []
numericBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String str) = let parsed = reads str in
                          if null parsed
                             then throwError $ TypeMismatch "number" $ String str
                          else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr s = throwError $ TypeMismatch "string" s

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool b = throwError $ TypeMismatch "boolean" b

symbolToString, stringToSymbol :: LispVal -> LispVal
symbolToString (Atom sym) = String sym
symbolToString _ = String ""
stringToSymbol (String str) = Atom str
stringToSymbol _ = Atom ""

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

