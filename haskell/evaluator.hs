module Evaluator where
import Boolean
import Control.Monad
import Control.Monad.Error
import Data.Maybe (isNothing)
import Environment
import LispValue
import ListPrimitives
import Numeric
import String
import Unpacker

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Character _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Number _) = return val
eval _ val@(Float _) = return val
eval _ val@(Ratio _) = return val
eval _ val@(Complex _) = return val
eval env (Atom id) = getVar env id
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
   result <- eval env pred
   case result of
     Bool False -> eval env alt
     Bool True -> eval env conseq
     otherwise -> throwError  $ TypeMismatch "boolean" result
eval env (List (Atom "cond" : args)) = evalCond env args
eval env (List (Atom "case" : exemplar : cases)) =
    evalCase env exemplar cases
eval env (List [Atom "set!", Atom var, form]) = eval env form >>=
    setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval _ badForm =
    throwError $ BadSpecialForm "Unrecognized Special Form" [badForm]

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && isNothing varargs
      then throwError $ NumArgs (num params) args
      else liftIO (bindVars closure $ zip params args) >>=
        bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env =
            case arg of
              Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
              Nothing -> return env

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
              ("string-ci=?", stringCI (==)),
              ("string<?", strBoolBinop (<)),
              ("strig-ci<?", stringCI (<)),
              ("string>?", strBoolBinop (>)),
              ("string-ci>?", stringCI (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string-ci<=?", stringCI (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("string-ci>=?", stringCI (>=)),
              ("string-length", stringLength),
              ("string-ref", stringRef),
              ("substring", substring),
              ("string-append", stringAppend),
              ("string->list", stringToList),
              ("list->string", listToString),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map makePrimitiveFunc primitives)
  where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

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

evalCond :: Env -> [LispVal] -> IOThrowsError LispVal
evalCond env (List [Atom "else", conseq] : _) = eval env conseq
evalCond env (List [pred, conseq] : rest) = do
    predResult <- eval env pred
    let (Bool truthy) = predResult in
        if truthy
        then eval env conseq
        else evalCond env rest
evalCond _ badArgs = throwError $ BadSpecialForm "improper cond" badArgs

evalCase :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalCase env _ (List [Atom "else", conseq] : _) = eval env conseq
evalCase env exemplar (List [target, conseq] : rest) = do
    predResult <- liftThrows $ eqv [exemplar, target]
    let (Bool truthy) = predResult in
      if truthy
      then eval env conseq
      else evalCase env exemplar rest
