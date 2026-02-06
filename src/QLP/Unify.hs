module QLP.Unify
  ( Subst
  , emptySubst
  , applyTerm
  , applyAtom
  , occurs
  , compose
  , unifyTerm
  , unifyList
  , unifyAtom
  ) where

import qualified Data.Map.Strict as M
import QLP.Syntax

type Subst = M.Map Name Term

emptySubst :: Subst
emptySubst = M.empty

applyTerm :: Subst -> Term -> Term
applyTerm s t = case t of
  TVar x -> case M.lookup x s of
    Nothing -> TVar x
    Just u  -> applyTerm s u
  TFun f xs -> TFun f (map (applyTerm s) xs)

occurs :: Name -> Term -> Bool
occurs x t = case t of
  TVar y     -> x == y
  TFun _ xs  -> any (occurs x) xs

applyAtom :: Subst -> Atom -> Atom
applyAtom s (Atom p xs) = Atom p (map (applyTerm s) xs)

-- Compose substitutions: apply s2, then s1 (so s1 has priority).
compose :: Subst -> Subst -> Subst
compose s1 s2 =
  let s2' = M.map (applyTerm s1) s2
  in M.union s1 s2'

bindVar :: Name -> Term -> Subst -> Maybe Subst
bindVar x t s =
  case applyTerm s (TVar x) of
    TVar y | y == x ->
      let t' = applyTerm s t
      in if occurs x t' then Nothing else Just (M.insert x t' s)
    TVar y ->
      -- x was already mapped to something else via s; unify that instead
      unifyTerm (applyTerm s (TVar y)) t s
    u ->
      unifyTerm u t s

unifyTerm :: Term -> Term -> Subst -> Maybe Subst
unifyTerm t1 t2 s =
  case (applyTerm s t1, applyTerm s t2) of
    (TVar x, t) -> bindVar x t s
    (t, TVar x) -> bindVar x t s
    (TFun f xs, TFun g ys)
      | f == g && length xs == length ys ->
          unifyList xs ys s
      | otherwise -> Nothing

unifyList :: [Term] -> [Term] -> Subst -> Maybe Subst
unifyList [] [] s = Just s
unifyList (x:xs) (y:ys) s = do
  s' <- unifyTerm x y s
  unifyList xs ys s'
unifyList _ _ _ = Nothing

unifyAtom :: Atom -> Atom -> Subst -> Maybe Subst
unifyAtom (Atom p xs) (Atom q ys) s
  | p == q && length xs == length ys = unifyList xs ys s
  | otherwise = Nothing
