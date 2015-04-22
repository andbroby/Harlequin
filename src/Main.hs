{-# LANGUAGE ExistentialQuantification #-}

module Main where

import System.Environment
import GHC.IO.Handle
import Data.IORef
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Char
import Data.Complex
import Data.Ratio
import qualified Data.Array as A
import Control.Monad.Error

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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String],
                      vararg :: (Maybe String),
                      body :: [LispVal],
                      closure :: Env
                    }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle                      

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

instance Show LispVal where
  show (String content) = "\"" ++ content ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Atom name) = name
  show (Number content) = show content
  show (List content) = "(" ++ unwordList content ++ ")"
  show (DottedList head tail) = "(" ++ unwordList head ++ " . " ++ show tail ++ ")"
  show (PrimitiveFunc _) = "<primitive>"
  show (Func {params = args, vararg = vararg, body = body, closure = env}) =
    "lambda (" ++ unwords (map show args) ++
    (case vararg of
      Nothing -> ""
      Just arg -> "." ++ arg) ++ ") ...)"
  show (Port _) = "<IO port>"
  show (IOFunc _) = "<IO primitive>"

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

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ErrorT LispError IO


main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] 
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) 
        >>= hPutStrLn stderr


runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Harlequin >>> ") . evalAndPrint

nullEnv :: IO Env
nullEnv = newIORef []

makeFunc varargs env params body = return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . show

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                 ++ map (makeFunc PrimitiveFunc) primitives)
                    where makeFunc constructor (var, func) = (var, constructor func)

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue 

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting to an unbound variable" var)
                               (liftIO . (flip writeIORef value))
                               (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
     then setVar envRef var value >> return value
     else liftIO $ do
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value     

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
                           where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
                                 addBinding (var, value) = do ref <- newIORef value
                                                              return (var, ref)
                                                              
trapError action = catchError action (return . show)

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
    else action result >> until_ pred prompt action

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
    
unwordList :: [LispVal] -> String
unwordList = unwords . map show

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr parseNumber)

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

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do result <- eval env pred
     case result of
      Bool False -> eval env alt
      Bool True -> eval env conseq
      otherwise -> throwError $ TypeMismatch "bool" pred
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env form@(List (Atom "cond" : clauses)) =
  if null clauses
     then throwError $ BadSpecialForm "no clause for true in cond expr" form
  else
    case head clauses of
     List [Atom "else", expr] -> eval env expr
     List [test, expr] -> eval env $ List [Atom "if",
                                       test,
                                       expr,
                                       List (Atom "cond" : tail clauses)]
     otherwise -> throwError $ BadSpecialForm "ill-formed cond expr: " form
eval env (List (Atom "define" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeVarArgs varargs env [] body
eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form " badForm
eval env (List [Atom "load", String filename]) = 
     load filename >>= liftM last . mapM (eval env)
    
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
     then throwError $ NumArgs (num params) args
  else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
          Nothing -> return env
apply (IOFunc func) args = func args

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

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
eqv [l1@(List arg1), l2@(List arg2)] = eqvList eqv [l1, l2]
eqv [_, _] = return $ Bool $ False
eqv x = throwError $ NumArgs 2 x

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvf [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                          (all eqvPair$ zip arg1 arg2)
                                          where eqvPair (x1, x2) = case eqv [x1, x2] of
                                                  Left err -> False
                                                  Right (Bool b) -> b

equal :: [LispVal] -> ThrowsError LispVal
equal [l1@(List arg1), l2@(List arg2)] = eqvList equal [l1, l2]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal err = throwError $ NumArgs 2 err

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <-unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)
     

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
              ("string>=?", strBoolBinOp (>=)),
              ("car" , car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)
             ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

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

