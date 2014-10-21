module LispValue where
import Data.Complex
import Data.Ratio
import Data.Vector (Vector, fromList, toList)

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

unwordsLisp :: [LispVal] -> String
unwordsLisp = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Ratio contents) = show contents
showVal (Complex contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsLisp contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsLisp head ++ " . " ++ showVal tail  ++ ")"
showVal (Vector contents) = "#(" ++ unwordsLisp (toList contents) ++ ")"


instance Show LispVal where
    show = showVal
