module Syntax where

import Prelude hiding (concat, sequence)
import Parser
import Control.Applicative hiding ((<|>))
import Control.Monad.Trans.Either
import Control.Monad.State.Strict hiding (sequence)

import Data
import Data.List (find)

type Bindings = [[String]]

type Syntax a = EitherT String (State Bindings) (a, List LispData)

data Ast = Let [String] [Ast] Ast
         | Definition String Ast
         | LocalReference String
         | FreeReference String
         | Literal LispData
         | If Ast Ast Ast
         | Seq [Ast] deriving (Show)

newtype Checker a = Checker { runChecker :: List LispData -> Syntax a }

instance Monad Checker where
  checker >>= f = Checker bind where
    bind xs =
      let syntax = runChecker checker xs in
      EitherT $ do
        a <- runEitherT syntax
        case a of
          Left e -> return $ Left e
          Right (x, rest) -> runEitherT $ runChecker (f x) rest
  return x = Checker (\xs -> return (x, xs))

instance Applicative Checker where
  pure = return
  (<*>) = ap

instance Functor Checker where
  fmap f checker = Checker c where
    c xs =
      let syntax = runChecker checker xs in
      EitherT $ do
        a <- runEitherT syntax
        case a of
          Left e -> return $ Left e
          Right (x, rest) -> return $ Right (f x, rest)

lispLet :: Checker Ast
lispLet = sexpr $ do
  void $ symbol "let"
  b <- sexpr bindings
  pushFrame (fst <$> b)
  body <- sequence
  popFrame
  return (Let (fst <$> b) (snd <$> b) (Seq body))

bindings :: Checker [(String, Ast)]
bindings = zeroMany $ list2 binding lispExpr
  where
  list2 :: Checker a -> Checker b -> Checker (a, b)
  list2 ac bc = sexpr $ do
    a <- ac
    b <- bc
    return (a, b)

  binding :: Checker String
  binding = name <$> anySymbol
    where name (Symbol s) = s
          name _ = "Fail. Should not happen."

addBinding :: String -> Checker ()
addBinding name = Checker c
  where c xs = state (\s -> (((), xs), newState name s))
        newState x [] = [[x]]
        newState x (frame:prev) = (x : frame) : prev

pushFrame :: [String] -> Checker ()
pushFrame names = Checker c
  where c xs = state (\prev -> (((), xs), names : prev))

popFrame :: Checker ()
popFrame = Checker c
  where c xs = state (\s -> (((), xs), pop s))
        pop [] = []
        pop (_:xs) = xs

definition :: Checker Ast
definition = sexpr $ do
  void $ symbol "def"
  (Symbol name) <- anySymbol
  expr <- lispExpr
  addBinding name
  return $ Definition name expr

sequence :: Checker [Ast]
sequence = zeroMany lispExpr

doBlock :: Checker Ast
doBlock = sexpr $ do
  void $ symbol "do"
  xs <- sequence
  return $ Seq xs

reference :: Checker Ast
reference = do
  (Symbol name) <- anySymbol
  addRef name
    where
    addRef :: String -> Checker Ast
    addRef name = Checker f where
      f xs = do
        s <- get
        return $ maybe (FreeReference name, xs) (const (LocalReference name, xs)) $ find (elem name) s

-- @todo Won't work? State from c1 is being used in c2
(<&>) :: Checker a -> Checker b -> Checker b
(<&>) c1 c2 = Checker f where
  f xs =
    let syntax = runChecker c1 xs in
    EitherT $ do
      a <- runEitherT syntax
      case a of
        Left e -> return $ Left e
        Right _ -> runEitherT $ runChecker c2 xs

-- the 'or' operator
(<|>) :: Checker a -> Checker a -> Checker a
(<|>) c1 c2 = Checker f where
  f xs =
    let syntax = runChecker c1 xs in
    EitherT $ do
      a <- runEitherT syntax
      case a of
        Left _ -> runEitherT $ runChecker c2 xs
        Right x -> return $ Right x

anyVal :: Checker LispData
anyVal = Checker f where
  f Null = left "Expecting a value, but got nothing"
  f (Pair x rest) = return (x, rest)

sexpr :: Checker a -> Checker a
sexpr c = anyVal <&> Checker f where
  f (Pair x rest) = case x of
    (Sexpr xs) -> (\(res, _) -> (res, rest)) <$> runChecker c xs
    _ -> left $ "Expecting a list, got " ++ show x
  f _ = left ""

zeroMany :: Checker a -> Checker [a]
zeroMany c = Checker p where
  p l = EitherT $ loop [] l where
    loop acc Null = return $ Right (acc, Null)
    loop acc rest = do
      a <- runEitherT (runChecker c rest)
      case a of
        Left e -> return $ Left e
        Right (x, xs) -> loop (acc ++ [x]) xs

check :: (LispData -> Bool) -> String -> Checker LispData
check f msg = Checker checker where
  checker (Pair x rest) = if f x then return (x, rest) else left $ msg ++ ", got " ++ show x
  checker _ = left "Fail. This should not happen."

anySymbol ::  Checker LispData
anySymbol = anyVal <&> check sym "Expecting any symbol" where
  sym (Symbol _) = True
  sym _ = False

symbol ::  String -> Checker LispData
symbol name = anySymbol <&> check named ("Expecting symbol " ++ name) where
  named (Symbol sym) = sym == name
  named _ = False

string :: Checker LispData
string = check s "Expecting a string" where
  s (Str _) = True
  s _ = False

literal :: Checker Ast
literal = Literal <$> check val "Expecting a literal" where
  val (LispBool _) = True
  val (Number _) = True
  val (Str _) = True
  val (LispMap _) = True
  val (Keyword _) = True
  val _ = False

lispExpr :: Checker Ast
lispExpr = literal <|> definition <|> reference <|> lispLet <|> doBlock

parse :: Checker LispData -> String -> Either String LispData
parse c input = case readLispData input of
  Ok e _ -> either Left (Right . fst) $ evalState (runEitherT (runChecker c (Pair e Null))) []
  Fail -> Left "Parse error"

parseExpr :: String -> Either String Ast
parseExpr input = case readLispData input of
  Ok e _ -> either Left (Right . fst) $ evalState (runEitherT (runChecker lispExpr (Pair e Null))) []
  Fail -> Left "Parse error"

runExprState' :: Checker Ast -> String -> Either String Bindings
runExprState' checker input = case readLispData input of
  Ok e _ -> case runState (runEitherT (runChecker checker (Pair e Null))) [] of
    (Left m, _) -> Left m
    (Right _, s) -> Right s
  Fail -> Left "Parse error"

runExprState :: String -> Either String Bindings
runExprState = runExprState' lispExpr
