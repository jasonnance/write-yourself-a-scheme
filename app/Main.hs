module Main where

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

main :: IO ()
main = do args <- getArgs
          putStrLn $ "Parsing: " ++ head args
          putStrLn (readExpr (head args))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

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

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> try parseNumber
            <|> try parseBool
            <|> try parseChar
            <|> parseQuoted
            <|> do _ <- char '('
                   x <- try parseList <|> parseDottedList
                   _ <- char ')'
                   return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"

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
