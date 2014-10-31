{-# LANGUAGE ExistentialQuantification #-}

module LispValue where
import Control.Monad.Error
import Data.Complex
import Data.IORef
import Data.Ratio
import Data.Vector (Vector, fromList, toList)
import Text.ParserCombinators.Parsec hiding (spaces)


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | Ratio Rational
             | Complex (Complex Float)
             | Bool Bool
             | String String
             | Character Char
             | Vector (Vector LispVal)
             | PrimitiveFun ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String],
                      varargs :: Maybe String,
                      body :: [LispVal],
                      closure :: Env
                    }

type Env = IORef[(String, IORef LispVal)]

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String [LispVal]
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO

data Unpacker = forall a. Eq a => AnyUnpacker(LispVal -> ThrowsError a)

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Character char) = show char
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Ratio contents) = show contents
showVal (Complex contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail  ++ ")"
showVal (Vector contents) = "#(" ++ unwordsList (toList contents) ++ ")"

instance Show LispVal where
    show = showVal


showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "expected: " ++ show expected ++
  " found values: " ++ unwordsList found
showError (TypeMismatch expected found) = "expected: " ++ expected ++
  " found: " ++ show found
showError (Parser error) = "Parse error at: " ++ show error
showError (Default error) = show error

instance Show LispError where
    show = showError

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
