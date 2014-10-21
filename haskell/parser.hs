module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.Complex
import Data.Ratio
import System.Environment
import Numeric (readOct, readHex, readFloat)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | Ratio Rational
             | Complex (Complex Float)
             | Bool Bool
             | String String
             deriving Show

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (head args))

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseEscape :: Parser Char
parseEscape = do
    _ <- char '\\'
    escape <- oneOf "\"ntr\\"
    return $ case escape of
               '"' -> '"'
               '\\' -> '\\'
               'n' -> '\n'
               't' -> '\t'
               'r' -> '\r'
               _ -> '\0'

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many (parseEscape <|> noneOf "\"")
    _ <- char '"'
    return $ String x

parseBinary :: Parser LispVal
parseBinary = do
    x <- many1 $ oneOf "01"
    return (Number . readBinary $ x)
    where c2i c = if c == '0' then 0 else 1
          readBinary = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i

parseOctal :: Parser LispVal
parseOctal = do
    x <- many1 $ oneOf "01234567"
    let [(num, _)] = readOct x
    return $ Number num

parseFloat :: Parser LispVal
parseFloat = do
    fst <- many1 digit
    _ <- char '.'
    rst <- many digit
    let [(num, _)] = readFloat (fst ++ "." ++ rst)
    return $ Float num

parseDecimal :: Parser LispVal
parseDecimal = try parseFloat <|> liftM (Float . fst . head . readFloat) (many1 digit)

parseHex :: Parser LispVal
parseHex = do
    x <- many1 (digit <|> oneOf "abcdefABCDEF")
    let [(num, _)] = readHex x
    return $ Number num

parseInteger :: Parser LispVal
parseInteger = liftM (Number . read)  (many1 digit)

prefixedNumber :: Parser LispVal
prefixedNumber = do
    prefix <- try (string "#b")
              <|> try (string "#d")
              <|> try (string "#o")
              <|> try (string "#x")
    case prefix of
       "#b" -> parseBinary
       "#d" -> parseDecimal
       "#o" -> parseOctal
       "#x" -> parseHex

parseRatio :: Parser LispVal
parseRatio = do
    x <- many1 digit
    _ <- char '/'
    y <- many1 digit
    return $ Ratio (read x % read y)

parseComplex :: Parser LispVal
parseComplex = do Float x <- try parseDecimal
                  char '+'
                  Float y <- try parseDecimal
                  char 'i'
                  return $ Complex (x :+ y)

parseNumber :: Parser LispVal
-- parseNumber = many1 digit >>= return . Number . read
-- parseNumber = do
--     num <- many1 digit
--     return (Number . read $ num)
parseNumber = prefixedNumber
              <|> try parseComplex
              <|> try parseRatio
              <|> try parseFloat
              <|> parseInteger

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom

parseBoolean :: Parser LispVal
parseBoolean = do
    c <- oneOf "tf"
    return $ case c of
               't' -> Bool True
               'f' -> Bool False

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =  parseString
            <|> parseNumber
            <|> parseAtom
            <|> parseQuoted
            <|> do
                 _ <-  char '('
                 x <- try parseList <|> parseDottedList
                 _ <-char ')'
                 return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match " ++ show err
                   Right val -> "Found value " ++ show val
