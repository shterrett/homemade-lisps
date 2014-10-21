module Main where
import Evaluator (eval)
import LispValue
import Parser
import System.Environment
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> String $ "No match " ++ show err
                   Right val -> val
