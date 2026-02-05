module QLP.Search where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import QLP.Syntax
import QLP.Unify

-- A Horn-like rule: Head :- Body1, ..., Bodyk.
data Rule = Rule
  { headAtom :: Atom
  , bodyAtoms :: [Atom]
  } deriving (Eq, Show, Read)


type Program = [Rule]
type Comm = Atom -> Atom -> Bool

-- Depth-first search for QLP goals (stub commutativity).
-- We keep a fresh counter for rename-apart like before.
solveQLP :: Comm -> QProgram -> Goal -> Subst -> Int -> [Subst]
solveQLP _ _ (Goal [] []) s _ = [s]
solveQLP comm prog (Goal (r:rs) ns) s k =
  concatMap step prog
  where
    step c0 =
      let (c, k1) = renameClause k c0
      in case pickMatchPos r (posAtoms c) s of
           Nothing -> []
           Just (_matched, s', restPos) ->
             let newPosGoals = map (applyAtom s') (negAtoms c) ++ map (applyAtom s') rs
                 newNegGoals = map (applyAtom s') restPos ++ map (applyAtom s') ns
                 atomsToCheck = applyAtom s' r : (newPosGoals ++ newNegGoals)
                 g' = Goal newPosGoals newNegGoals
             in if not (commutesAllAtoms comm atomsToCheck) then [] else solveQLP comm prog g' s' k1

solveQLP comm prog (Goal [] (sAtom:ns)) s k =
  concatMap step prog
  where
    step c0 =
      let (c, k1) = renameClause k c0
      in case pickMatchPos sAtom (negAtoms c) s of
           Nothing -> []
           Just (_matched, s', restNeg) ->
             let newPosGoals = map (applyAtom s') (posAtoms c)
                 newNegGoals = map (applyAtom s') restNeg ++ map (applyAtom s') ns
                 atomsToCheck = applyAtom s' sAtom : (newPosGoals ++ newNegGoals)
                 g' = Goal newPosGoals newNegGoals
             in if not (commutesAllAtoms comm atomsToCheck) then [] else solveQLP comm prog g' s' k1

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = [(x,y) | y <- xs] ++ pairs xs

commutesAllAtoms :: Comm -> [Atom] -> Bool
commutesAllAtoms comm atoms = all (\(a,b) -> comm a b) (pairs atoms)


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
  { negAtoms :: [Atom]  -- p1,...,pm in (￢p1 ∨ ... ∨ ￢pm ∨ q1 ∨ ... ∨ qn)
  , posAtoms :: [Atom]  -- q1,...,qn
  } deriving (Eq, Show, Read)


data Goal = Goal
  { wantPos :: [Atom]   -- r1,...,rm  in (r1 ∧ ... ∧ rm ∧ ￢s1 ∧ ... ∧ ￢sn)
  , wantNeg :: [Atom]   -- s1,...,sn
  } deriving (Eq, Show, Read)

type QProgram = [Clause]



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
