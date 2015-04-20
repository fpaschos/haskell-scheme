module Main where

import Control.Monad
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

--Exercise 2,3
escapedChars :: Parser Char
escapedChars = do char '\\' 
                  x <- oneOf "\\\"nrt" 
                  return $ case x of 
                    '\\' -> x
                    '"'  -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'
                    
parseString :: Parser LispVal
parseString  = do char '"'
                  x <- many $ escapedChars <|> noneOf "\"\\"
                  char '"'
                  return $ String x

-- Exercise 4 (only implemented for hex) 
parseNumber :: Parser LispVal
parseNumber = parseDigital1 <|> parseHex

parseDigital1 :: Parser LispVal
parseDigital1 = many1 digit >>= return . Number . read

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 digit
              return $ Number (hex2dig x)

hex2dig x = fst $ readHex x !! 0

-- Exercise 1
-- parseNumber = liftM (Number . read) $ many1 digit

-- parseNumber = do
  -- digits <- many1 digit
  -- let n = read digits
  -- return (Number n) 

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))


parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseBool

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t
  
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input  of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value " 

main ::IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))  
