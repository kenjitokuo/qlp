module QLP.Search
  ( Rule(..)
  , Program
  , Comm
  , Clause(..)
  , Goal(..)
  , QProgram
  , solve
  , solveQLP
  , solveQLPWithDebug
  , pairs
  , commutesAllAtoms
  , pickMatchPos
  , varsClause
  , renameClause
  , varsTerm
  , varsAtom
  , varsRule
  , renameTerm
  , renameAtom
  , renameRule
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import QLP.Syntax
import QLP.Unify
import Debug.Trace (trace)

data Rule = Rule { headAtom :: Atom, bodyAtoms :: [Atom] } deriving (Eq, Show, Read)

type Program = [Rule]
type Comm = Atom -> Atom -> Bool

solveQLP :: Comm -> QProgram -> Goal -> Subst -> Int -> [Subst]
solveQLP = solveQLPWithDebug False

solveQLPWithDebug :: Bool -> Comm -> QProgram -> Goal -> Subst -> Int -> [Subst]
solveQLPWithDebug dbg comm prog goal s k = solveQLP' dbg comm prog goal s k []

solveQLP' :: Bool -> Comm -> QProgram -> Goal -> Subst -> Int -> [(Atom, Atom)] -> [Subst]
solveQLP' dbg comm _ (Goal [] []) s _ pending =
  case normalizePending comm s pending of
    Left (aBad, bBad) -> debugIf dbg ("[comm-fail final] pair=" ++ show aBad ++ " / " ++ show bBad) []
    Right pending' ->
      case pending' of
        [] -> [s]
        (a', b'):_ ->
          if pendingMustBeGroundAtEnd
            then debugIf dbg ("[comm-pending final nonground] " ++ show a' ++ " / " ++ show b') []
            else debugIf dbg ("[comm-pending final nonground] " ++ show a' ++ " / " ++ show b') [s]

solveQLP' dbg comm prog (Goal (r:rs) ns) s k pending =
  concatMap step prog
  where
    step c0 =
      let (c, k1) = renameClause k c0
      in case pickMatchPos r (posAtoms c) s of
           Nothing -> []
           Just (_matched, s', restPos) ->
             let newPosGoals = map (applyAtom s') (negAtoms c) ++ map (applyAtom s') rs
                 newNegGoals = map (applyAtom s') restPos ++ map (applyAtom s') ns
                 g'          = Goal newPosGoals newNegGoals
                 sel         = applyAtom s' r
                 ctx         = newPosGoals ++ newNegGoals
                 pending0    = addPendingPairs sel ctx pending
             in case normalizePending comm s' pending0 of
                  Left (aBad, bBad) -> debugIf dbg ("[comm-fail pos] k=" ++ show k1 ++ " pair=" ++ show aBad ++ " / " ++ show bBad ++ " ctx=" ++ show ctx) []
                  Right pending'    -> solveQLP' dbg comm prog g' s' k1 pending'

solveQLP' dbg comm prog (Goal [] (sAtom:ns)) s k pending =
  concatMap step prog
  where
    step c0 =
      let (c, k1) = renameClause k c0
      in case pickMatchPos sAtom (negAtoms c) s of
           Nothing -> []
           Just (_matched, s', restNeg) ->
             let newPosGoals = map (applyAtom s') (posAtoms c)
                 newNegGoals = map (applyAtom s') restNeg ++ map (applyAtom s') ns
                 g'          = Goal newPosGoals newNegGoals
                 sel         = applyAtom s' sAtom
                 ctx         = newPosGoals ++ newNegGoals
                 pending0    = addPendingPairs sel ctx pending
             in case normalizePending comm s' pending0 of
                  Left (aBad, bBad) -> debugIf dbg ("[comm-fail neg] k=" ++ show k1 ++ " pair=" ++ show aBad ++ " / " ++ show bBad ++ " ctx=" ++ show ctx) []
                  Right pending'    -> solveQLP' dbg comm prog g' s' k1 pending'

isGroundTerm :: Term -> Bool
isGroundTerm t = case t of { TVar _ -> False; TFun _ xs -> all isGroundTerm xs }

isGroundAtom :: Atom -> Bool
isGroundAtom (Atom _ xs) = all isGroundTerm xs

dedupe :: Eq a => [a] -> [a]
dedupe = go [] where go acc [] = reverse acc; go acc (x:xs) = if x `elem` acc then go acc xs else go (x:acc) xs

addPendingPairs :: Atom -> [Atom] -> [(Atom, Atom)] -> [(Atom, Atom)]
addPendingPairs sel ctx pending = dedupe ([(sel, s) | s <- ctx] ++ pending)

normalizePending :: Comm -> Subst -> [(Atom, Atom)] -> Either (Atom, Atom) [(Atom, Atom)]
normalizePending comm s pending = go [] pending
  where
    go acc [] = Right (dedupe (reverse acc))
    go acc ((a,b):ps) =
      let a' = applyAtom s a
          b' = applyAtom s b
      in if isGroundAtom a' && isGroundAtom b'
           then if comm a' b' then go acc ps else Left (a', b')
           else go ((a', b') : acc) ps

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = [(x,y) | y <- xs] ++ pairs xs

commutesAllAtoms :: Comm -> [Atom] -> Bool
commutesAllAtoms comm atoms = all (\(a,b) -> comm a b) (pairs atoms)

pendingMustBeGroundAtEnd :: Bool
pendingMustBeGroundAtEnd = False

debugIf :: Bool -> String -> a -> a
debugIf True msg x = trace msg x
debugIf False _ x = x

pickMatchPos :: Atom -> [Atom] -> Subst -> Maybe (Atom, Subst, [Atom])
pickMatchPos _ [] _ = Nothing
pickMatchPos a (b:bs) s =
  case unifyAtom a b s of
    Just s' -> Just (b, s', bs)
    Nothing -> do (b', s', rest) <- pickMatchPos a bs s; Just (b', s', b:rest)

varsClause :: Clause -> S.Set Name
varsClause (Clause ns ps) = S.unions (map varsAtom (ns ++ ps))

renameClause :: Int -> Clause -> (Clause, Int)
renameClause k c =
  let vs = S.toList (varsClause c)
      m = M.fromList [ (x, x ++ "_" ++ show i) | (x, i) <- zip vs [k..] ]
      c' = Clause (map (renameAtom m) (negAtoms c)) (map (renameAtom m) (posAtoms c))
      k' = k + length vs
  in (c', k')

data Clause = Clause { negAtoms :: [Atom], posAtoms :: [Atom] } deriving (Eq, Show, Read)
data Goal = Goal { wantPos :: [Atom], wantNeg :: [Atom] } deriving (Eq, Show, Read)

type QProgram = [Clause]

varsTerm :: Term -> S.Set Name
varsTerm t = case t of { TVar x -> S.singleton x; TFun _ xs -> S.unions (map varsTerm xs) }

varsAtom :: Atom -> S.Set Name
varsAtom (Atom _ xs) = S.unions (map varsTerm xs)

varsRule :: Rule -> S.Set Name
varsRule (Rule h bs) = S.unions (varsAtom h : map varsAtom bs)

renameTerm :: M.Map Name Name -> Term -> Term
renameTerm m t = case t of { TVar x -> TVar (M.findWithDefault x x m); TFun f xs -> TFun f (map (renameTerm m) xs) }

renameAtom :: M.Map Name Name -> Atom -> Atom
renameAtom m (Atom p xs) = Atom p (map (renameTerm m) xs)

renameRule :: Int -> Rule -> (Rule, Int)
renameRule k r =
  let vs = S.toList (varsRule r)
      m = M.fromList [ (x, x ++ "_" ++ show i) | (x, i) <- zip vs [k..] ]
      r' = Rule (renameAtom m (headAtom r)) (map (renameAtom m) (bodyAtoms r))
      k' = k + length vs
  in (r', k')

solve :: Program -> [Atom] -> Subst -> Int -> [Subst]
solve _ [] s _ = [s]
solve prog (g:gs) s k =
  concatMap (step k) prog
  where
    step k0 r0 =
      let (r, k1) = renameRule k0 r0
      in case unifyAtom g (headAtom r) s of
           Nothing -> []
           Just s' -> let newGoals = map (applyAtom s') (bodyAtoms r) ++ map (applyAtom s') gs in solve prog newGoals s' k1
