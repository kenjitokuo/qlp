module QLP.Unify where

import qualified Data.Map.Strict as M
import QLP.Syntax

type Subst = M.Map Name Term

emptySubst :: Subst
emptySubst = M.empty
