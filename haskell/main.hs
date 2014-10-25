module Main where
import Control.Monad
import Control.Monad.Error
import Error
import Evaluator (eval)
import LispValue
import Parser
import System.Environment
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = do
    args <- getArgs
    let evaled = liftM show $ readExpr (head args) >>= eval
    putStrLn $ extractValue $ trapError evaled

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> throwError $ Parser err
                   Right val -> return val
