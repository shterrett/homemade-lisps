module Unpacker where
import Control.Monad.Error
import LispValue

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackString :: LispVal -> ThrowsError String
unpackString (String str) = return str
unpackString (Number n) = return $ show n
unpackString (Bool b) = return $ show b
unpackString notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool
