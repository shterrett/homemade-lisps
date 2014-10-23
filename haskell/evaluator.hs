module Evaluator where
import LispValue
import Numeric

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Character _) = val
eval val@(Bool _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Ratio _) = val
eval val@(Complex _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
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
              ("string->symbol", symbolToString)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

isSymbol :: [LispVal] -> LispVal
isSymbol [Atom _] = Bool True
isSymbol _  = Bool False

isString :: [LispVal] -> LispVal
isString [String _] = Bool True
isString _ = Bool False

isBoolean :: [LispVal] -> LispVal
isBoolean [Bool _] = Bool True
isBoolean _ = Bool False

isList :: [LispVal] -> LispVal
isList [List _] = Bool True
isList _ = Bool False

isNumber :: [LispVal] -> LispVal
isNumber [Number _] = Bool True
isNumber [Float _] = Bool True
isNumber [Ratio _] = Bool True
isNumber [Complex _] = Bool True
isNumber _ = Bool False

isCharacter :: [LispVal] -> LispVal
isCharacter [Character _] = Bool True
isCharacter _ = Bool False

isVector :: [LispVal] -> LispVal
isVector [Vector _] = Bool True
isVector _ = Bool False

symbolToString :: [LispVal] -> LispVal
symbolToString [Atom atom] = String string
symbolToString _ = String ""

stringToSymbol :: [LispVal] -> LispVal
stringToSymbol [String str] = Atom str
stringToSymbol _ = Atom ""
