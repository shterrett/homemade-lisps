module Evaluator where
import Control.Monad
import Control.Monad.Error
import Error
import LispValue
import Numeric

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Character _) = return val
eval val@(Bool _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Ratio _) = return val
eval val@(Complex _) = return val
eval (List [Atom "quote", val]) = return val
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
              ("string>=?", strBoolBinop (>=))]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                               then throwError $ NumArgs 2 args
                               else do left <- unpacker $ head args
                                       right <- unpacker $ args !! 1
                                       return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackString
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackString :: LispVal -> ThrowsError String
unpackString (String str) = return str
unpackString (Number n) = return $ show n
unpackString (Bool b) = return $ show b
unpackString notString = throwError $ TypeMismatch "not string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "not boolean" notBool

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol _  = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString _ = return $ Bool False

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean [Bool _] = return $ Bool True
isBoolean _ = return $ Bool False

isList :: [LispVal] -> ThrowsError LispVal
isList [List _] = return $ Bool True
isList _ = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ Bool True
isNumber [Float _] = return $ Bool True
isNumber [Ratio _] = return $ Bool True
isNumber [Complex _] = return $ Bool True
isNumber _ = return $ Bool False

isCharacter :: [LispVal] -> ThrowsError LispVal
isCharacter [Character _] = return $ Bool True
isCharacter _ = return $ Bool False

isVector :: [LispVal] -> ThrowsError LispVal
isVector [Vector _] = return $ Bool True
isVector _ = return $ Bool False

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom atom] = return $ String atom
symbolToString _ = return $ String ""

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String str] = return $ Atom str
stringToSymbol _ = return $ Atom ""
