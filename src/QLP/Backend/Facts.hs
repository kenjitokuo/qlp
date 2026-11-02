module QLP.Backend.Facts
  ( mkCommFacts
  ) where

-- Facts backend: comm(u,v) is true iff u and v have the same predicate symbol or the
-- (unordered) pair of their predicate symbols is listed in the facts file.
-- Arguments of the atoms are ignored.

import qualified Data.Set as S
import QLP.Syntax (Atom(..))
import QLP.Search (Comm)

-- The fact set is normalized to predicate names and stored symmetrically, once.
mkCommFacts :: [(Atom, Atom)] -> Comm
mkCommFacts facts0 =
  let factSet = S.fromList (concat [ [(px, py), (py, px)] | (Atom px _, Atom py _) <- facts0 ])
  in \(Atom pa _) (Atom pb _) -> pa == pb || S.member (pa, pb) factSet

