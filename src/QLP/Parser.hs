module QLP.Parser (
  parseQProgramText,
  parseGoalText,
  parseCommFactsText,
  parseAtomText
) where

import QLP.Syntax (Term(..), Atom(..))
import QLP.Search (Clause(..), Goal(..), QProgram)
import Data.Char (isAlphaNum, isSpace, toLower, isUpper)
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Text.Read (readMaybe)

parseQProgramText :: String -> Either String QProgram
parseQProgramText s = parseAllOrRead qprogP s "QProgram"

parseGoalText :: String -> Either String Goal
parseGoalText s = parseAllOrRead goalP s "Goal"

parseCommFactsText :: String -> Either String [(Atom, Atom)]
parseCommFactsText s = parseAllOrRead factsP s "CommFacts"

-- Atom text parser (useful for comm-check CLI etc.)
parseAtomText :: String -> Either String Atom
parseAtomText s = parseAll atomP s "Atom"

parseAllOrRead :: Read a => ReadP a -> String -> String -> Either String a
parseAllOrRead p s what =
  case parseAll p s what of
    Right x -> Right x
    Left _  -> case readMaybe s of
                 Just x  -> Right x
                 Nothing -> Left ("Parse error: " ++ what)

parseAll :: ReadP a -> String -> String -> Either String a
parseAll p s what =
  case [x | (x, rest) <- readP_to_S (skipWS *> p <* skipWS <* eof) s, all isSpace rest] of
    (x:_) -> Right x
    []    -> Left ("Parse error: " ++ what)

-- whitespace + comments (# ... , % ...)
skipWS :: ReadP ()
skipWS = skipMany (skipSpaces1 <|> comment) where
  skipSpaces1 = satisfy isSpace *> pure ()
  comment = do
    _ <- char '#' <|> char '%'
    _ <- munch (/= '\n')
    _ <- optional (char '\n')
    pure ()

lexeme :: ReadP a -> ReadP a
lexeme p = skipWS *> p <* skipWS

symbol :: String -> ReadP String
symbol t = lexeme (string t)

kw :: String -> ReadP String
kw w = lexeme $ do
  let cs = map toLower w
  xs <- count (length cs) (satisfy isAlphaNum)
  if map toLower xs == cs then pure xs else pfail

ident :: ReadP String
ident = lexeme $ do
  c  <- satisfy (\x -> isAlphaNum x || x == '_' )
  cs <- munch (\x -> isAlphaNum x || x == '_' )
  pure (c:cs)

-- Term: Var (Upper or _) | Fun/Const (otherwise). Const is Fun with 0 args.
termP :: ReadP Term
termP = varP <|> funP where
  varP = do
    x <- ident
    if isVarName x then pure (TVar x) else pfail
  funP = do
    f <- ident
    if isVarName f then pfail else do
      margs <- option Nothing (Just <$> parens (sepBy termP (symbol ",")))
      pure $ case margs of { Nothing -> TFun f []; Just as -> TFun f as }

isVarName :: String -> Bool
isVarName [] = False
isVarName (c:_) = isUpper c || c == '_'

parens :: ReadP a -> ReadP a
parens p = do { _ <- symbol "("; x <- p; _ <- symbol ")"; pure x }

atomP :: ReadP Atom
atomP = do
  p <- ident
  margs <- option Nothing (Just <$> parens (sepBy termP (symbol ",")))
  pure $ Atom p (maybe [] id margs)

-- Literal: "not" Atom | Atom
litP :: ReadP (Either Atom Atom)
litP = negP <|> posP where
  negP = do { _ <- kw "not"; a <- atomP; pure (Left a) }
  posP = do { a <- atomP; pure (Right a) }

-- Clause syntax (paper-like): lit1 ; lit2 ; ... .
-- negAtoms = atoms preceded by "not", posAtoms = others.
clauseP :: ReadP Clause
clauseP = do
  ls <- sepBy1 litP (symbol ";")
  _  <- optional (symbol ".")
  let negs = [a | Left a <- ls]
  let poss = [a | Right a <- ls]
  pure (Clause negs poss)

qprogP :: ReadP QProgram
qprogP = many clauseP

-- Goal syntax: ?- lit1, lit2, ... .
-- wantPos = atoms without "not", wantNeg = atoms with "not".
goalP :: ReadP Goal
goalP = do
  _ <- optional (lexeme (string "?-"))
  ls <- sepBy litP (symbol ",")
  _  <- optional (symbol ".")
  let negs = [a | Left a <- ls]
  let poss = [a | Right a <- ls]
  pure (Goal poss negs)

-- commutativity facts: comm(Atom, Atom).
factP :: ReadP (Atom, Atom)
factP = do
  _ <- kw "comm"
  _ <- symbol "("
  a <- atomP
  _ <- symbol ","
  b <- atomP
  _ <- symbol ")"
  _ <- optional (symbol ".")
  pure (a,b)

factsP :: ReadP [(Atom, Atom)]
factsP = many factP
