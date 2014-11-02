module Main where
import Control.Monad
import Control.Monad.Error
import Environment
import Evaluator (eval, primitiveBindings)
import LispIO
import LispValue
import Parser
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    if null args then runRepl else runOne args

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
runRepl = primitiveBindings >>= until_ (== "(exit)") (readPrompt "Lisp >>> ") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows
      (liftM show $
        eval env (List [Atom "load", String (head args)]))
        >>= hPutStrLn stderr
