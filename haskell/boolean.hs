module Boolean where
import Control.Monad.Error
import LispValue
import Unpacker

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

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
          do unpacked1 <- unpacker arg1
             unpacked2 <- unpacker arg2
             return $ unpacked1 == unpacked2
        `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [List arg1, List arg2] = return $ Bool $
                               length arg1 == length arg2 &&
                               all equalPair (zip arg1 arg2)
  where equalPair (p1, p2) = case equal [p1, p2] of
                               Left err -> False
                               Right (Bool val) -> val
equal [arg1, arg2] = do
  equals <- liftM or $ mapM (unpackEquals arg1 arg2)
                            [AnyUnpacker unpackNum,
                             AnyUnpacker unpackString,
                             AnyUnpacker unpackBool]
  return $ Bool equals
equal badArgsList = throwError $ NumArgs 2 badArgsList
