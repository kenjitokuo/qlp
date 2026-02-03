module QLP.Backend.Hilbert where

import QLP.Syntax

-- A handle to a Hilbert-space backend (finite-dimensional model).
-- For now, commutativity always returns True; we will implement it later.
data HilbertModel = HilbertModel
  { dim :: Int
  } deriving (Eq, Show)

defaultModel :: HilbertModel
defaultModel = HilbertModel { dim = 2 }

-- Interpret an atom as a projection (placeholder).
-- Later: user-provided interpretation + matrix construction.
commutes :: HilbertModel -> Atom -> Atom -> Bool
commutes _ _ _ = True
