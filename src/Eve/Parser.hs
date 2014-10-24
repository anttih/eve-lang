module Eve.Parser where

import Prelude hiding (sequence, lookup)
import Control.Applicative
import Control.Monad.Trans.Either
import Control.Monad.State.Strict hiding (sequence)
import Data.List (find)

import Eve.Data
import Eve.Reader

data Binding = Binding String | Special String deriving (Show)

type Bindings = [[Binding]]

type Syntax a = EitherT (String, List LispData) (State Bindings) (a, List LispData)

data Ast = Let [Binding] [Ast] Ast
         | Function [Binding] Ast
         | Application Ast [Ast]
         | Definition String Ast
         | LocalReference String
         | FreeReference String
         | Literal LispData
         | Alternative Ast Ast Ast
         | Seq [Ast] deriving (Show)

newtype Parser a = Parser { runParser :: List LispData -> Syntax a }

instance Monad Parser where
  checker >>= f = Parser bind where
    bind xs =
      let syntax = runParser checker xs in
      EitherT $ do
        a <- runEitherT syntax
        case a of
          Left e -> return $ Left e
          Right (x, rest) -> runEitherT $ runParser (f x) rest
  return x = Parser (\xs -> return (x, xs))

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Alternative Parser where
  empty = Parser (\_ -> left ("", Null))
  (<|>) c1 c2 = Parser f where
    f rest = 
      let syntax = runParser c1 rest in
      EitherT $ do
        a <- runEitherT syntax
        case a of
          Left (_, rest') | rest == rest' -> runEitherT $ runParser c2 rest
          Left e -> return $ Left e
          Right res -> return $ Right res

instance Functor Parser where
  fmap f checker = Parser c where
    c xs =
      let syntax = runParser checker xs in
      EitherT $ do
        a <- runEitherT syntax
        case a of
          Left e -> return $ Left e
          Right (x, rest) -> return $ Right (f x, rest)

binding :: LispData -> Binding
binding (Symbol s) = Binding s
binding _ = Binding ""

-- State modification
bindings :: Parser [(Binding, Ast)]
bindings = many $ sexpr $ (,) <$> toBinding <*> lispExpr where
  toBinding = binding <$> anySymbol

addBinding :: String -> Parser ()
addBinding name = Parser c where
  c xs = state (\s -> (((), xs), newState name s)) where
    newState x [] = [[Binding x]]
    newState x (frame:prev) = (Binding x : frame) : prev

pushFrame :: [Binding] -> Parser ()
pushFrame names = Parser c where
  c xs = state $ \prev -> (((), xs), names : prev)

popFrame :: Parser ()
popFrame = Parser c where
  c xs = state $ \s -> (((), xs), pop s) where
    pop [] = []
    pop (_:rest) = rest

lispLet :: Parser Ast
lispLet = do
  void $ symbol "let"
  b <- sexpr bindings
  pushFrame (fst <$> b)
  body <- sequence
  popFrame
  return $ Let (fst <$> b) (snd <$> b) (Seq body)

sequence :: Parser [Ast]
sequence = many lispExpr

definition :: Parser Ast
definition = do
  void $ symbol "def"
  (Symbol name) <- anySymbol
  expr <- lispExpr
  addBinding name
  return $ Definition name expr

funcDefinition :: Parser Ast
funcDefinition = do
  void $ symbol "defn"
  (Symbol name) <- anySymbol
  addBinding name
  params <- sexpr $ many $ binding <$> anySymbol
  pushFrame params
  impl <- sequence
  popFrame
  return $ Definition name (Function params (Seq impl))

doBlock :: Parser Ast
doBlock = Seq <$> (symbol "do" *> sequence)

application :: Parser Ast
application = do
  fun <- reference
  args <- some lispExpr
  return $ Application fun args

reference :: Parser Ast
reference = do
  (Symbol name) <- anySymbol
  getRef name

getRef :: String -> Parser Ast
getRef name = Parser f where
  f xs = do
    s <- get
    return $ maybe (FreeReference name, xs) (const (LocalReference name, xs)) $ lookup name s

lookup :: String -> [[Binding]] -> Maybe Binding
lookup name = find elemBinding . concat where
  elemBinding (Binding s) | name == s = True
  elemBinding (Special s) | name == s = True
  elemBinding _                       = False

-- @todo Won't work? State from c1 is being used in c2
(<&>) :: Parser a -> Parser b -> Parser b
(<&>) c1 c2 = Parser f where
  f xs =
    let syntax = runParser c1 xs in
    EitherT $ do
      a <- runEitherT syntax
      case a of
        Left e -> return $ Left e
        Right _ -> runEitherT $ runParser c2 xs

anyVal :: Parser LispData
anyVal = Parser f where
  f Null = left ("Expecting a value, but got nothing", Null)
  f (Pair x rest) = return (x, rest)

endSexpr :: Parser ()
endSexpr = Parser f where
  f Null          = return ((), Null)
  f (Pair x rest) = left ("Expecting the end of list, but got " ++ show x, rest)

-- Parse values from inside a lisp list (...) with checker `c`. The checker
-- should consume all of the values inside the list.
sexpr :: Parser a -> Parser a
sexpr c = anyVal <&> Parser isSexpr where
  isSexpr orig@(Pair x rest) = case x of
    (Sexpr xs) -> (\(res, _) -> (res, rest)) <$> runParser (c <* endSexpr) xs
    _ -> left ("Expecting a list, got " ++ show x, orig)
  isSexpr _ = left ("", Null)

check :: (LispData -> Bool) -> String -> Parser LispData
check f msg = Parser checker where
  checker orig@(Pair x rest) = if f x then return (x, rest) else left (msg ++ ", got " ++ show x, orig)
  checker _ = left ("Fail. This should not happen.", Null)

anySymbol ::  Parser LispData
anySymbol = anyVal <&> check sym "Expecting any symbol" where
  sym (Symbol _) = True
  sym _ = False

symbol ::  String -> Parser LispData
symbol name = anySymbol <&> check named ("Expecting symbol " ++ name) where
  named (Symbol sym) = sym == name
  named _ = False

string :: Parser LispData
string = check s "Expecting a string" where
  s (Str _) = True
  s _ = False

literal :: Parser Ast
literal = Literal <$> check val "Expecting a literal" where
  val (LispBool _) = True
  val (Number _) = True
  val (Str _) = True
  val (LispMap _) = True
  val (Keyword _) = True
  val _ = False

lispForm :: Parser Ast
lispForm = sexpr $ definition <|> funcDefinition <|> lispLet <|> doBlock <|> application

lispExpr :: Parser Ast
lispExpr = literal <|> reference <|> lispForm

parse :: Parser LispData -> String -> Either String LispData
parse c input = case readLispData input of
  Ok e _ -> either (Left . fst) (Right . fst) $ evalState (runEitherT (runParser c (Pair e Null))) []
  Fail -> Left "Parse error"

parseExpr :: String -> Either String Ast
parseExpr input = case readLispData input of
  Ok e _ -> either (Left . fst) (Right . fst) $ evalState (runEitherT (runParser lispExpr (Pair e Null))) []
  Fail -> Left "Parse error"

runExprState' :: Parser Ast -> String -> Either String Bindings
runExprState' checker input = case readLispData input of
  Ok e _ -> case runState (runEitherT (runParser checker (Pair e Null))) [] of
    (Left (m, _), _) -> Left m
    (Right _, s) -> Right s
  Fail -> Left "Parse error"

runExprState :: String -> Either String Bindings
runExprState = runExprState' lispExpr
