module Boolean where
import Control.Monad.Error
import Error
import LispValue

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

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool x, Bool y] = return $ Bool $ x == y
eqv [Number x, Number y] = return $ Bool $ x == y
eqv [String x, String y] = return $ Bool $ x == y
eqv [Atom x, Atom y] = return $ Bool $ x == y
eqv [DottedList xs x, DottedList ys y] =
    eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [List xs, List ys] = return $ Bool $
  (length xs == length ys) &&
  all equivPair (zip xs ys)
  where equivPair (px, py) = case eqv [px, py] of
                            Left err -> False
                            Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgs = throwError $ NumArgs 2 badArgs

