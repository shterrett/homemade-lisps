module Evaluator where
import Boolean
import Control.Monad
import Control.Monad.Error
import Error
import LispValue
import ListPrimitives
import Numeric
import Unpacker

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Character _) = return val
eval val@(Bool _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Ratio _) = return val
eval val@(Complex _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
   result <- eval pred
   case result of
     Bool False -> eval alt
     Bool True -> eval conseq
     otherwise -> throwError  $ TypeMismatch "boolean" result
eval (List (Atom "cond" : args)) = evalCond args
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized Special Form" [badForm]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                  ($ args)
                  (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", isSymbol),
              ("string?", isString),
              ("boolean?", isBoolean),
              ("list?", isList),
              ("number?", isNumber),
              ("character?", isCharacter),
              ("vector?", isVector),
              ("symbol->string", symbolToString),
              ("string->symbol", symbolToString),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = liftM (Number . foldl1 op) (mapM unpackNum params)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                               then throwError $ NumArgs 2 args
                               else do left <- unpacker $ head args
                                       right <- unpacker $ args !! 1
                                       return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackString
boolBoolBinop = boolBinop unpackBool

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom atom] = return $ String atom
symbolToString _ = return $ String ""

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String str] = return $ Atom str
stringToSymbol _ = return $ Atom ""

evalCond :: [LispVal] -> ThrowsError LispVal
evalCond (List [Atom "else", conseq] : _) = eval conseq
evalCond (List [pred, conseq] : rest) = do
    predResult <- eval pred
    let (Bool truthy) = predResult in
        if truthy
        then eval conseq
        else evalCond rest
evalCond badArgs = throwError $ BadSpecialForm "improper cond" badArgs
