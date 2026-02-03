module QLP.Search where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import QLP.Syntax
import QLP.Unify

-- A Horn-like rule: Head :- Body1, ..., Bodyk.
data Rule = Rule
  { headAtom :: Atom
  , bodyAtoms :: [Atom]
  } deriving (Eq, Show)

type Program = [Rule]

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
