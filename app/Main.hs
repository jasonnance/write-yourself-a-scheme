module Main where

import Control.Monad.Except
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("bool?", unaryOp boolp),
              ("symbol?", unaryOp symbolp),
              ("number?", unaryOp numberp),
              ("string?", unaryOp stringp),
              ("list?", unaryOp listp),
              ("symbol->string", unaryOp symbolString)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

boolp :: LispVal -> LispVal
boolp (Bool _) = Bool True
boolp _ = Bool False

symbolp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _ = Bool False

numberp :: LispVal -> LispVal
numberp (Number _) = Bool True
numberp _ = Bool False

stringp :: LispVal -> LispVal
stringp (String _) = Bool True
stringp _ = Bool False

listp :: LispVal -> LispVal
listp (List _) = Bool True
listp (DottedList _ _) = Bool True
listp _ = Bool False

symbol2String, string2Symbol :: LispVal -> LispVal
symbol2String (Atom s) = String s
symbol2String _ = String ""
string2Symbol (String s) = Atom s
string2Symbol _ = Atom ""

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser String
escapedChars = do _ <- char '\\'
                  x <- oneOf "\\\"ntr"
                  case x of
                    '\\' -> return [x]
                    '"' -> return [x]
                    't' -> return "\t"
                    'n' -> return "\n"
                    'r' -> return "\r"

parseChar :: Parser LispVal
parseChar = do _ <- try $ string "#\\"
               x <- parseCharName <|> anyChar
               return $ Character x

parseCharName :: Parser Char
parseCharName = do x <- try (string "space" <|> string "newline")
                   case x of
                     "space" -> return ' '
                     "newline" -> return '\n'

-- parseString :: Parser LispVal
-- parseString = do _ <- char '"'
--                  x <- many (noneOf "\""
--                             <|> (last <$> string "\\\""))
--                  _ <- char '"'
--                  return $ String x


parseString :: Parser LispVal
parseString = do _ <- char '"'
                 x <- many $ many1 (noneOf "\"\\") <|>
                      escapedChars
                 _ <- char '"'
                 return $ String (concat x)

parseBool :: Parser LispVal
parseBool = do _ <- string "#"
               x <- oneOf "tf"
               return $ case x of
                          't' -> Bool True
                          'f' -> Bool False

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ Atom atom

parseDigital1 :: Parser LispVal
parseDigital1 = do x <- many1 digit
                   (return . Number . read) x

parseDigital2 :: Parser LispVal
parseDigital2 = do _ <- try $ string "#d"
                   x <- many1 digit
                   (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do _ <- try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do _ <- try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do _ <- try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

parseNumber :: Parser LispVal
parseNumber = parseDigital1
                <|> parseDigital2
                <|> parseHex
                <|> parseOct
                <|> parseBin

-- parseNumber :: Parser LispVal
-- parseNumber = do val <- many1 digit
--                  return $ Number $ read val

-- parseNumber :: Parser LispVal
-- parseNumber = many1 digit >>= (return . Number . read)

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  dotHead <- endBy parseExpr spaces
  dotTail <- char '.' >> spaces >> parseExpr
  return $ DottedList dotHead dotTail

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  _ <- char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuoted :: Parser LispVal
parseUnQuoted = do
  _ <- char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnQuotedSplicing :: Parser LispVal
parseUnQuotedSplicing = do
  _ <- string ",@"
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> try parseNumber
            <|> try parseBool
            <|> try parseChar
            <|> parseQuoted
            <|> parseQuasiQuoted
            <|> parseUnQuoted
            <|> parseUnQuotedSplicing
            <|> do _ <- char '('
                   x <- try parseList <|> parseDottedList
                   _ <- char ')'
                   return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

oct2dig :: String -> Integer
oct2dig x = fst $ head (readOct x)

hex2dig :: String -> Integer
hex2dig x = fst $ head (readHex x)

bin2dig :: String -> Integer
bin2dig = bin2dig' 0

bin2dig' :: Integer -> String -> Integer
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1)
                             in bin2dig' old xs
