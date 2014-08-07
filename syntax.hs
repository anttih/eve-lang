module Syntax where

import qualified Parser as S
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy

type ExpansionResult a = ExceptT String (State ()) a

type Bindings = [String]

data Ast = Let [String] [Ast] Ast
         | If Ast Ast Ast
         | Seq [Ast]

-- lispSpecial :: String -> S.Sexpr -> ExpansionResult S.Sexpr
-- lispSpecial name sexpr = case sexpr of
--     (S.List (S.Pair (S.Symbol sym) rest)) | sym == name -> return (S.List rest)
--     _ -> fail "failure"
-- 
-- lispLet :: S.Sexpr -> ExpansionResult Ast
-- lispLet s = do
--   (S.List (S.Pair b (S.Pair body _))) <- lispSpecial "let" s
--   b <- bindings b
--   seq <- thunk body
--   return (Let ["moi"] [] seq)
-- 
-- bindings :: S.Sexpr -> ExpansionResult S.Sexpr
-- bindings = list $ zeroMany $ list2 symbol any

thunk :: S.Sexpr -> ExpansionResult Ast
thunk _ = return $ Seq []

-- checkLet :: MaybeT ExpansionState Int
-- checkLet = do (S.List p) <- lispSpecial "let"
--               return p

data Result = Ok | Error String deriving (Show)

-- checker :: Sexpr -> Result

both ::  (t -> Result) -> (t -> Result) -> t -> Result
both c1 c2 sexpr = case c1 sexpr
                   of Ok -> c2 sexpr
                      e -> e

any :: S.Sexpr -> ExpansionResult S.Sexpr
any = return

list :: S.Sexpr -> ExpansionResult S.Sexpr
list (S.List l) = return (S.List l)
list e = fail $ "Expecting a list, got " ++ show e

-- zeroMany :: (S.Sexpr -> ExpansionResult S.Sexpr) -> S.List' -> ExpansionResult S.Sexpr
-- zeroMany _ Null = return Null
-- zeroMany f p = case f p of
 

anySymbol ::  S.Sexpr -> Result
anySymbol (S.Symbol _) = Ok
anySymbol s = Error $ "Not a symbol, got " ++ show s

symbol ::  String -> S.Sexpr -> Result
symbol name = both anySymbol checkSymbol where
  checkSymbol (S.Symbol sym) = if sym == name
                               then Ok
                               else Error $ "Not symbol " ++ name ++ ", got " ++ sym
  checkSymbol _ = Error "Expecting a symbol"

nullList ::  S.Sexpr -> Result
nullList (S.List S.Null) = Ok
nullList s = Error $ "Expecting an empty list, got " ++ show s

isPair ::  S.Sexpr -> Result
isPair (S.List S.Null) = Error "Not expecting an empty list"
isPair (S.List (S.Pair _ _)) = Ok
isPair s = Error $ "Expecting a non empty list, got" ++ show s

pair :: (S.Sexpr -> Result) -> (S.Sexpr -> Result) -> S.Sexpr -> Result
pair first rest = both isPair checkPair where
  checkPair (S.List (S.Pair x xs)) = case (first x, rest (S.List xs))
                                   of (Ok, Ok) -> Ok
                                      (Error m, _) -> Error m
                                      (Ok, Error m) -> Error m
  checkPair _ = Error "Expecting a pair"

--data Checker a = Checker (a -> Result)
--
--instance Monad Checker where
--  (Checker c) >>= f = Checker bind where
--    bind (S.List (S.Pair x xs)) = case c x
--                                  of Ok -> case f x
--                                           of (Checker ch) -> ch (S.List xs)
--                                     e -> e
--  return v = (\l -> Ok)

-- letForm = do symbol "let"
--              list (zeroMany bindings)
--              thunk


checkString :: (S.Sexpr -> Result) -> String -> Either String S.Sexpr
checkString c s = check (S.parse S.expr s) where
  check (S.Ok sexpr _) = case c sexpr
                          of Ok -> Right sexpr
                             (Error msg) -> Left msg
  check S.Fail = Left "Parse error"

-- forms (S.List (S.Pair (S.Symbol "let") rest)) =
-- forms (S.List (S.Pair (S.Symbol "if") rest)) =
