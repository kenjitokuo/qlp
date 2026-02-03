module QLP.Search where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import QLP.Syntax
import QLP.Unify
import QLP.Backend.Hilbert

-- A Horn-like rule: Head :- Body1, ..., Bodyk.
data Rule = Rule
  { headAtom :: Atom
  , bodyAtoms :: [Atom]
  } deriving (Eq, Show)

type Program = [Rule]

-- Depth-first search for QLP goals (stub commutativity).
-- We keep a fresh counter for rename-apart like before.
solveQLP :: HilbertModel -> QProgram -> Goal -> Subst -> Int -> [Subst]
solveQLP _ _ (Goal [] []) s _ = [s]
solveQLP model prog (Goal (r:rs) ns) s k =
  -- try to solve positive atom r using a clause where r matches some q_k
  concatMap step prog
  where
    step c0 =
      let (c, k1) = renameClause k c0
      in case pickMatchPos r (posAtoms c) s of
         Nothing -> []
         Just (matched, s', restPos) ->
           if not (commutes model r matched) then [] else
             let newPosGoals = map (applyAtom s') (negAtoms c) ++ map (applyAtom s') rs
                 newNegGoals = map (applyAtom s') restPos ++ map (applyAtom s') ns
             in solveQLP model prog (Goal newPosGoals newNegGoals) s' k1

solveQLP model prog (Goal [] (sAtom:ns)) s k =
  -- try to solve negative goal ÊsAtom using a clause where sAtom matches some p_k (i.e., negAtoms)
  concatMap step prog
  where
    step c0 =
      let (c, k1) = renameClause k c0
      in case pickMatchPos sAtom (negAtoms c) s of
         Nothing -> []
         Just (matched, s', restNeg) ->
           if not (commutes model sAtom matched) then [] else
             let newPosGoals = map (applyAtom s') (posAtoms c)  -- add q's positively
                 newNegGoals = map (applyAtom s') restNeg ++ map (applyAtom s') ns
             in solveQLP model prog (Goal newPosGoals newNegGoals) s' k1

-- Unify an atom against one atom in a list; return (newSubst, remainingAtoms).
pickMatchPos :: Atom -> [Atom] -> Subst -> Maybe (Atom, Subst, [Atom])
pickMatchPos _ [] _ = Nothing
pickMatchPos a (b:bs) s =
  case unifyAtom a b s of
    Just s' -> Just (b, s', bs)
    Nothing -> do
      (b', s', rest) <- pickMatchPos a bs s
      Just (b', s', b:rest)

-- rename-apart for Clause using the existing renameTerm/renameAtom logic
varsClause :: Clause -> S.Set Name
varsClause (Clause ns ps) = S.unions (map varsAtom (ns ++ ps))

renameClause :: Int -> Clause -> (Clause, Int)
renameClause k c =
  let vs = S.toList (varsClause c)
      m = M.fromList [ (x, x ++ "_" ++ show i) | (x, i) <- zip vs [k..] ]
      c' = Clause (map (renameAtom m) (negAtoms c)) (map (renameAtom m) (posAtoms c))
      k' = k + length vs
  in (c', k')


-- QLP-style clause and goal (very small core)
data Clause = Clause
  { negAtoms :: [Atom]  -- p1,...,pm in (Êp1 É ... É Êpm É q1 É ... É qn)
  , posAtoms :: [Atom]  -- q1,...,qn
  } deriving (Eq, Show)

data Goal = Goal
  { wantPos :: [Atom]   -- r1,...,rm  in (r1 È ... È rm È Ês1 È ... È Êsn)
  , wantNeg :: [Atom]   -- s1,...,sn
  } deriving (Eq, Show)

type QProgram = [Clause]

-- Commutativity check stub (always true for now)
commutesWithProgram :: HilbertModel -> QProgram -> Atom -> Atom -> Bool
commutesWithProgram model _ a b = commutes model a b


-- Collect variables
varsTerm :: Term -> S.Set Name
varsTerm t = case t of
  TVar x -> S.singleton x
  TFun _ xs -> S.unions (map varsTerm xs)

varsAtom :: Atom -> S.Set Name
varsAtom (Atom _ xs) = S.unions (map varsTerm xs)

varsRule :: Rule -> S.Set Name
varsRule (Rule h bs) = S.unions (varsAtom h : map varsAtom bs)

-- Rename variables in a rule apart (fresh) using an integer counter
renameTerm :: M.Map Name Name -> Term -> Term
renameTerm m t = case t of
  TVar x -> TVar (M.findWithDefault x x m)
  TFun f xs -> TFun f (map (renameTerm m) xs)

renameAtom :: M.Map Name Name -> Atom -> Atom
renameAtom m (Atom p xs) = Atom p (map (renameTerm m) xs)

renameRule :: Int -> Rule -> (Rule, Int)
renameRule k r =
  let vs = S.toList (varsRule r)
      m = M.fromList [ (x, x ++ "_" ++ show i) | (x, i) <- zip vs [k..] ]
      r' = Rule (renameAtom m (headAtom r)) (map (renameAtom m) (bodyAtoms r))
      k' = k + length vs
  in (r', k')

-- Depth-first search with backtracking, returning all solutions.
-- The Int parameter is a fresh-name counter for rename-apart.
solve :: Program -> [Atom] -> Subst -> Int -> [Subst]
solve _ [] s _ = [s]
solve prog (g:gs) s k =
  concatMap (step k) prog
  where
    step k0 r0 =
      let (r, k1) = renameRule k0 r0
      in case unifyAtom g (headAtom r) s of
           Nothing -> []
           Just s' ->
             let newGoals = map (applyAtom s') (bodyAtoms r) ++ map (applyAtom s') gs
             in solve prog newGoals s' k1
