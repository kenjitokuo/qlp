module QLP.Syntax where

-- A minimal first-order term language for unification and goals.

type Name = String

data Term
  = TVar Name
  | TFun Name [Term]
  deriving (Eq, Ord, Show)

data Atom = Atom Name [Term]
  deriving (Eq, Ord, Show)

data Lit
  = Pos Atom
  | Neg Atom
  deriving (Eq, Ord, Show)
