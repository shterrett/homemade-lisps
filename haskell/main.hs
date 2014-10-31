module Main where
import Control.Monad
import Control.Monad.Error
import Environment
import Evaluator (eval)
import LispValue
import Parser
import System.IO
import System.Environment
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = do
    args <- getArgs
    case length args of
      1 -> runOne $ head args
      0 -> runRepl
      otherwise -> putStrLn "0 or 1 arguments required"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> throwError $ Parser err
                   Right val -> return val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env str = evalString env str >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    unless (pred result) $ action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "(exit)") (readPrompt "Lisp >>> ") . evalAndPrint

runOne :: String -> IO ()
runOne str = nullEnv >>= flip evalAndPrint str
