import qualified Parser as S
import Data.Either

data Result = Ok | Error String deriving (Show)

-- checker :: Sexpr -> Result

both c1 c2 sexpr = case c1 sexpr
                   of Ok -> c2 sexpr
                      e -> e

anySymbol (S.Symbol sym) = Ok
anySymbol s = Error $ "Not a symbol, got " ++ show s

symbol name = both anySymbol checkSymbol where
  checkSymbol (S.Symbol sym) = if sym == name
                               then Ok
                               else Error $ "Not symbol " ++ name ++ ", got " ++ sym

nullList (S.List []) = Ok
nullList s = Error $ "Expecting an empty list, got " ++ show s

nonEmptyList (S.List []) = Error "Not expecting an empty list"
nonEmptyList (S.List _) = Ok
nonEmptyList s = Error $ "Expecting a non empty list, got" ++ show s

pair first rest = both nonEmptyList checkPair where
  checkPair (S.List (x:xs)) = case (first x, rest (S.List xs))
                              of (Ok, Ok) -> Ok
                                 ((Error m), _) -> Error m
                                 (Ok, (Error m)) -> Error m

checkString :: (S.Sexpr -> Result) -> String -> Either String S.Sexpr
checkString c s = check (S.parse S.expr s) where
  check (S.Ok sexpr cs) = case (c sexpr)
                          of Ok -> Right sexpr
                             (Error msg) -> Left msg
  check S.Fail = Left "Parse error"
