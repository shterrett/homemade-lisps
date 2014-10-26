{-# LANGUAGE ExistentialQuantification #-}

module Evaluator where
import Boolean
import Control.Monad
import Control.Monad.Error
import Error
import LispValue
import ListPrimitives
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
eval (List [Atom "if", pred, conseq, alt]) = do
   result <- eval pred
   case result of
     Bool False -> eval alt
     Bool True -> eval conseq
     otherwise -> throwError  $ TypeMismatch "boolean" result
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

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom atom] = return $ String atom
symbolToString _ = return $ String ""

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String str] = return $ Atom str
stringToSymbol _ = return $ Atom ""

data Unpacker = forall a. Eq a => AnyUnpacker(LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
          do unpacked1 <- unpacker arg1
             unpacked2 <- unpacker arg2
             return $ unpacked1 == unpacked2
        `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                                       [AnyUnpacker unpackNum, AnyUnpacker unpackString, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgsList = throwError $ NumArgs 2 badArgsList
