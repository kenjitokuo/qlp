module Main where

import QLP.Syntax
import QLP.Unify
import QLP.Search
import QLP.Backend.Hilbert
import qualified Data.Map.Strict as M

showSubst :: Subst -> String
showSubst s =
  let pairs = [x ++ " -> " ++ show t | (x,t) <- M.toList s]
  in "{" ++ unwords pairs ++ "}"
showSubstFor :: [Name] -> Subst -> String
showSubstFor xs s =
  let pairs = [x ++ " -> " ++ show (applyTerm s (TVar x)) | x <- xs]
  in "{" ++ unwords pairs ++ "}"


main :: IO ()
main = do
  putStrLn "QLP unify smoke test"

  let t1 = TFun "f" [TVar "X", TFun "a" []]
  let t2 = TFun "f" [TFun "b" [], TVar "Y"]

  case unifyTerm t1 t2 emptySubst of
    Nothing -> putStrLn "unifyTerm: fail"
    Just s  -> putStrLn ("unifyTerm: ok " ++ showSubst s)

  let a1 = Atom "P" [TVar "X", TFun "g" [TVar "X"]]
  let a2 = Atom "P" [TFun "c" [], TVar "Z"]

  case unifyAtom a1 a2 emptySubst of
    Nothing -> putStrLn "unifyAtom: fail"
    Just s  -> putStrLn ("unifyAtom: ok " ++ showSubst s)


  let cyc1 = TVar "X"
  let cyc2 = TFun "f" [TVar "X"]
  case unifyTerm cyc1 cyc2 emptySubst of
    Nothing -> putStrLn "occurs-check: ok (rejected)"
    Just s  -> putStrLn ("occurs-check: unexpected " ++ showSubst s)

  putStrLn "Search smoke test (Horn subset)"

  -- parent(alice,bob). parent(bob,carol).
  -- ancestor(X,Y) :- parent(X,Y).
  -- ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).
  let parentAB = Rule (Atom "parent" [TFun "alice" [], TFun "bob" []]) []
  let parentBC = Rule (Atom "parent" [TFun "bob" [], TFun "carol" []]) []
  let anc1 = Rule (Atom "ancestor" [TVar "X", TVar "Y"]) [Atom "parent" [TVar "X", TVar "Y"]]
  let anc2 = Rule (Atom "ancestor" [TVar "X", TVar "Y"]) [Atom "parent" [TVar "X", TVar "Z"], Atom "ancestor" [TVar "Z", TVar "Y"]]
  let prog = [parentAB, parentBC, anc1, anc2]

  let q = [Atom "ancestor" [TFun "alice" [], TVar "Y"]]
  let sols = take 5 (solve prog q emptySubst 0)
  mapM_ (putStrLn . showSubstFor ["Y"]) sols

  putStrLn "Search smoke test (QLP stub, commutativity always true)"

  -- Clause: ÊP(X) É Q(X)   (meaning: P(X) -> Q(X))
  let c1 = Clause [Atom "P" [TVar "X"]] [Atom "Q" [TVar "X"]]
  -- Fact: P(a)
  let c2 = Clause [] [Atom "P" [TFun "a" []]]
  let qprog = [c1, c2]

  let g = Goal [Atom "Q" [TVar "Y"]] []

  model <- loadModelFromFile "hilbert.conf"

  let qsols = take 3 (solveQLP model qprog g emptySubst 0)
  mapM_ (putStrLn . showSubstFor ["Y"]) qsols

  putStrLn "Hilbert commutativity quick check"
  print (commutes model (Atom "P" []) (Atom "Q" []))
  print (commutes model (Atom "P" []) (Atom "R" []))
