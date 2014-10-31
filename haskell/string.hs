module String where
import Control.Monad.Error
import Data.Char (toLower)
import LispValue

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [String str] = return $ Number $ toInteger (length str)
stringLength [arg] = throwError $ TypeMismatch "string" arg
stringLength badArgs = throwError $ NumArgs 1 badArgs

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [String str, Number k] = return $ Character $ str !! fromInteger k
stringRef [arg, Number _] = throwError $ TypeMismatch "string" arg
stringRef [String _, arg] = throwError $ TypeMismatch "integer" arg
stringRef badArgs = throwError $ NumArgs 1 badArgs

stringCI :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
stringCI cmp [String x, String y] = return $ Bool $ map toLower x `cmp` map toLower y
stringCI _ [arg, String _] = throwError $ TypeMismatch "string" arg
stringCI _ [String _, arg] = throwError $ TypeMismatch "string" arg
stringCI _ badArgs = throwError $ NumArgs 2 badArgs

substring :: [LispVal] -> ThrowsError LispVal
substring [String str, Number x, Number y] =
    let start = fromInteger x
        end = fromInteger y
        len = (end - start) in
        if 0 <= start && start < end && end <= length str
        then return $ String $ take len $ drop start str
        else throwError $ BadSpecialForm "index out of range" [Number x, Number y]
substring [arg, Number _, Number _] = throwError $ TypeMismatch "string" arg
substring [String _, arg, Number _] = throwError $ TypeMismatch "number" arg
substring [String _, Number _, arg] = throwError $ TypeMismatch "number" arg
substring badArgs = throwError $ NumArgs 3 badArgs

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend [String s1, String s2] = return $ String $ s1 ++ s2
stringAppend [arg, String _] = throwError $ TypeMismatch "string" arg
stringAppend [String _, arg] = throwError $ TypeMismatch "string" arg
stringAppend badArgs = throwError $ NumArgs 2 badArgs

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [String str] = return $ List $ map Character str
stringToList [arg] = throwError $ TypeMismatch "string" arg
stringToList badArgs = throwError $ NumArgs 1 badArgs

listToString :: [LispVal] -> ThrowsError LispVal
listToString [List lst] = return $ String $ foldl (\str char -> str ++ [char]) "" $ map (\(Character x) -> x) lst
listToString [arg] = throwError $ TypeMismatch "list" arg
listToString badArgs = throwError $ NumArgs 1 badArgs
