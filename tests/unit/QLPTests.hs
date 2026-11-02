module Main (main) where

-- Unit tests for the QLP core (unification, backchaining BC(I)-BC(IV), pending commutativity
-- obligations, failing-pair reporting), the text parser and the pauli/facts backends.
-- Run with:  cabal test   (or: runghc -isrc tests/unit/QLPTests.hs)

import qualified Data.Map.Strict as M
import Data.IORef
import Control.Monad (unless)
import System.Exit (exitFailure)

import QLP.Syntax
import QLP.Unify
import QLP.Search
import QLP.Parser
import QLP.Backend.Pauli
import QLP.Backend.Facts

-- ---------------------------------------------------------------------------
-- helpers

c0 :: String -> Term
c0 x = TFun x []

at :: String -> [Term] -> Atom
at = Atom

commAll :: Comm
commAll _ _ = True

-- u and v do not commute; everything else commutes.
commUV :: Comm
commUV (Atom p _) (Atom q _) = not ((p, q) `elem` [("u", "v"), ("v", "u")])

-- only atoms with the same predicate commute
commEqPred :: Comm
commEqPred (Atom p _) (Atom q _) = p == q

prog :: String -> QProgram
prog s = either error id (parseQProgramText s)

goal :: String -> Goal
goal s = either error id (parseGoalText s)

solveAll :: Comm -> QProgram -> Goal -> [Subst]
solveAll comm p g = solveQLPWithDebug False comm p g emptySubst 0

withReason :: Comm -> QProgram -> Goal -> ([Subst], Maybe (Atom, Atom))
withReason comm p g = solveQLPWithReason False comm p g emptySubst 0

valueOf :: Name -> Subst -> Term
valueOf x s = applyTerm s (TVar x)

model :: String -> (Int, PauliModel)
model s = either error id (parsePauliModelText s)

answers :: Name -> [Subst] -> [Term]
answers x = map (valueOf x)

-- ---------------------------------------------------------------------------
-- tiny test framework

type Counter = IORef (Int, Int)

check :: Counter -> String -> Bool -> IO ()
check ref name ok = do
  modifyIORef' ref (\(n, f) -> (n + 1, if ok then f else f + 1))
  putStrLn ((if ok then "  ok   " else "  FAIL ") ++ name)

checkEq :: (Eq a, Show a) => Counter -> String -> a -> a -> IO ()
checkEq ref name expected actual = do
  check ref name (expected == actual)
  unless (expected == actual) $
    putStrLn ("         expected: " ++ show expected ++ "\n         actual:   " ++ show actual)

section :: String -> IO ()
section s = putStrLn ("== " ++ s)

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  ref <- newIORef (0, 0)

  section "unification"
  checkEq ref "f(X,a) = f(b,Y)"
    (Just [("X", c0 "b"), ("Y", c0 "a")])
    (fmap M.toList (unifyTerm (TFun "f" [TVar "X", c0 "a"]) (TFun "f" [c0 "b", TVar "Y"]) emptySubst))
  checkEq ref "occurs check rejects X = f(X)" Nothing
    (unifyTerm (TVar "X") (TFun "f" [TVar "X"]) emptySubst)
  checkEq ref "different predicates do not unify" Nothing
    (unifyAtom (at "p" [c0 "a"]) (at "q" [c0 "a"]) emptySubst)
  checkEq ref "different arities do not unify" Nothing
    (unifyAtom (at "p" [c0 "a"]) (at "p" [c0 "a", c0 "b"]) emptySubst)
  checkEq ref "chained bindings are resolved" (Just (c0 "a"))
    (fmap (valueOf "X") (unifyList [TVar "X", TVar "Y"] [TVar "Y", c0 "a"] emptySubst))

  section "parser"
  checkEq ref "clause with negative and positive literals"
    [Clause [at "r" [TVar "X"]] [at "p" [TVar "X"]], Clause [] [at "r" [c0 "a"]]]
    (prog "# qprog\nnot r(X); p(X).\nr(a).\n")
  checkEq ref "goal with positive and negative literals"
    (Goal [at "p" [c0 "a"]] [at "q" [TVar "Y"]])
    (goal "?- p(a), not q(Y).")
  checkEq ref "'nothing(a)' is a positive atom, not 'not hing(a)'"
    [Clause [] [at "nothing" [c0 "a"]]]
    (prog "nothing(a).")
  checkEq ref "'notp' is a positive atom"
    (Goal [at "notp" []] [])
    (goal "?- notp.")
  checkEq ref "'not p' is a negative literal"
    (Goal [] [at "p" []])
    (goal "?- not p.")
  checkEq ref "comments (# and %) are ignored"
    [Clause [] [at "p" [c0 "a"]]]
    (prog "% c1\np(a). # c2\n")
  checkEq ref "nested terms and _variables"
    (Right (at "p" [TFun "f" [TVar "_X", c0 "b"]]))
    (parseAtomText "p(f(_X,b))")
  checkEq ref "comm facts"
    (Right [(at "A" [c0 "a"], at "B" [c0 "a"])])
    (parseCommFactsText "comm(A(a), B(a)).\n")

  section "minimal text-format example: not r(X); p(X). r(a). ?- p(a)."
  let pMin = prog "not r(X); p(X).\nr(a).\n"
  checkEq ref "always: one answer" 1 (length (solveAll commAll pMin (goal "?- p(a).")))

  section "verdicts: provable vs no-solutions"
  let pA = prog "u(a).\nv(a).\n"
  check ref "u(a) provable" (not (null (solveAll commUV pA (goal "?- u(a)."))))
  check ref "u(b) no solutions (always)" (null (solveAll commAll pA (goal "?- u(b).")))

  section "verdicts: not-applicable with comm_fail_pair (u,v)"
  let pB = prog "u(X); v(X).\nnot v(a).\n"
      gB = goal "?- u(a)."
  check ref "always: provable" (not (null (solveAll commAll pB gB)))
  let (solsB, failB) = withReason commUV pB gB
  check ref "noncommuting u/v: no solutions" (null solsB)
  checkEq ref "failing pair is (u(a), v(a))" (Just (at "u" [c0 "a"], at "v" [c0 "a"])) failB

  section "branch commutativity example: ClauseA/B/C, goal u(X)"
  let pE = prog "u(a); v(a).\nnot v(a).\nu(b).\n"
      gE = goal "?- u(X)."
  checkEq ref "always: X=a and X=b" [c0 "a", c0 "b"] (answers "X" (solveAll commAll pE gE))
  checkEq ref "u,v noncommuting: only X=b" [c0 "b"] (answers "X" (solveAll commUV pE gE))

  section "BC(I): positive goal, non-unit clause"
  -- clause  not n(X) ; p(X) ; q(X)  and goal p(a):
  -- new goal  wantPos=[n(a)], wantNeg=[q(a)], obligations p(a)~n(a), p(a)~q(a).
  let pI = [Clause [at "n" [TVar "X"]] [at "p" [TVar "X"], at "q" [TVar "X"]], Clause [] [at "n" [c0 "a"]], Clause [at "q" [c0 "a"]] []]
      gI = Goal [at "p" [c0 "a"]] []
  check ref "always: provable" (not (null (solveAll commAll pI gI)))
  checkEq ref "gated: fails on first obligation (p(a), n(a))"
    ([], Just (at "p" [c0 "a"], at "n" [c0 "a"])) (withReason commEqPred pI gI)
  -- without the clause for not q(a), the remaining negative goal q(a) cannot be closed
  check ref "remaining positive literal becomes a negative goal"
    (null (solveAll commAll (take 2 pI) gI))

  section "BC(II): negative goal, non-unit clause"
  -- clause  not s(X) ; not t(X) ; p(X)  and goal not s(a):
  -- new goal  wantPos=[t(a)] (remaining negative atoms), wantNeg=[p(a)] (positive atoms).
  let cII = Clause [at "s" [TVar "X"], at "t" [TVar "X"]] [at "p" [TVar "X"]]
      gII = Goal [] [at "s" [c0 "a"]]
      factT = Clause [] [at "t" [c0 "a"]]
      factNotP = Clause [at "p" [c0 "a"]] []
      factP = Clause [] [at "p" [c0 "a"]]
      factNotT = Clause [at "t" [c0 "a"]] []
  check ref "provable with t(a) and not p(a)" (not (null (solveAll commAll [cII, factT, factNotP] gII)))
  check ref "NOT provable with p(a) and not t(a) (polarity must not be swapped)"
    (null (solveAll commAll [cII, factP, factNotT] gII))
  checkEq ref "gated: fails on first obligation (s(a), t(a))"
    ([], Just (at "s" [c0 "a"], at "t" [c0 "a"])) (withReason commEqPred [cII, factT, factNotP] gII)
  -- regression: tests/demo/qprog_test_neg_nonunit.txt  (ZZI -> XII, XII  does not give not ZZI)
  check ref "not ZZI; XII.  XII.  ?- not ZZI.  has no solutions"
    (null (solveAll commAll (prog "not ZZI; XII.\nXII.\n") (goal "?- not ZZI.")))
  check ref "not ZZI; not XII.  XII.  ?- not ZZI.  is provable (always)"
    (not (null (solveAll commAll (prog "not ZZI; not XII.\nXII.\n") (goal "?- not ZZI."))))

  section "BC(III)/BC(IV): unit clauses generate no obligations"
  checkEq ref "BC(III): positive unit clause, gated comm" ([Just (c0 "a")], Nothing)
    (let (ss, fp) = withReason (\_ _ -> False) [Clause [] [at "p" [c0 "a"]]] (Goal [at "p" [TVar "X"]] [])
     in (map (Just . valueOf "X") ss, fp))
  check ref "BC(IV): negative unit clause, gated comm"
    (not (null (solveAll (\_ _ -> False) [Clause [at "p" [c0 "a"]] []] (Goal [] [at "p" [c0 "a"]]))))
  check ref "BC(III) requires no negative literal"
    (null (solveAll (\_ _ -> False) [Clause [at "n" []] [at "p" []], Clause [] [at "n" []]] (Goal [at "p" []] [])))

  section "goal selection: positive goals before negative goals"
  -- with a positive goal pending, a negative goal is selected only afterwards
  check ref "mixed goal succeeds"
    (not (null (solveAll commAll (prog "p(a).\nnot q(a).\n") (goal "?- not q(a), p(a)."))))

  section "pending obligations are checked once ground"
  -- clause  u(X) ; v(Y)  : obligation u(X)~v(Y) is nonground until Y is bound by a later step.
  let pP = prog "u(X); v(Y).\nnot v(a).\n"
  checkEq ref "nonground obligation checked after binding: fails with (u(a), v(a))"
    ([], Just (at "u" [c0 "a"], at "v" [c0 "a"])) (withReason commUV pP (goal "?- u(a)."))
  check ref "always: succeeds" (not (null (solveAll commAll pP (goal "?- u(a)."))))
  check ref "obligation that never becomes ground blocks success"
    (null (solveAll commAll (prog "u(X); v(Y).\nnot v(Z).\n") (goal "?- u(a).")))

  section "standardizing apart"
  -- a goal variable named like a renamed clause variable must not clash
  checkEq ref "goal variable X_0 does not clash with renamed clause variable"
    [TFun "f" [c0 "b"]]
    (answers "X_0" (solveAll commAll [Clause [] [at "p" [TVar "X", TFun "f" [TVar "X"]]]] (Goal [at "p" [c0 "b", TVar "X_0"]] [])))
  check ref "recursive clause is renamed on every use: anc(a,c)"
    (not (null (solveAll commAll
      (prog "par(a,b).\npar(b,c).\nnot par(X,Y); anc(X,Y).\nnot par(X,Z); not anc(Z,Y); anc(X,Y).\n")
      (goal "?- anc(a,c)."))))

  section "Horn subset: QLP behaves like SLD under always"
  let pAnc = prog "par(a,b).\npar(b,c).\nnot par(X,Y); anc(X,Y).\nnot par(X,Z); not anc(Z,Y); anc(X,Y).\n"
  checkEq ref "anc(a,Y): Y=b, Y=c" [c0 "b", c0 "c"] (answers "Y" (solveAll commAll pAnc (goal "?- anc(a,Y).")))

  section "pauli backend"
  checkEq ref "XII vs ZZI anticommute" (Right False) (commutesPauli "XII" "ZZI")
  checkEq ref "XX vs ZZ commute" (Right True) (commutesPauli "XX" "ZZ")
  checkEq ref "XYZ vs III commute" (Right True) (commutesPauli "XYZ" "III")
  checkEq ref "length mismatch is an error" True (either (const True) (const False) (commutesPauli "XI" "X"))
  let (n3, pm) = model "n = 3\nA(a) = XZI\nB(a) = IZI\nC(a) = IIZ\ngx = XXX\nu(a) = XII\nu(b) = ZII\nv = ZZI\ndim = 2\nq = ket0\n"
      cp = commPauli pm
  checkEq ref "n parsed" 3 n3
  checkEq ref "A(a) = XZI" (Just "XZI") (lookupPauli pm (at "A" [c0 "a"]))
  checkEq ref "entry without arguments (gx = XXX)" (Just "XXX") (lookupPauli pm (at "gx" []))
  checkEq ref "lowercase predicate (u(a) = XII)" (Just "XII") (lookupPauli pm (at "u" [c0 "a"]))
  checkEq ref "exact atom lookup (u(b) = ZII)" (Just "ZII") (lookupPauli pm (at "u" [c0 "b"]))
  checkEq ref "non-Pauli key=value lines are ignored" Nothing (lookupPauli pm (at "q" []))
  checkEq ref "comm(A(a),B(a)) = true (XZI, IZI)" True (cp (at "A" [c0 "a"]) (at "B" [c0 "a"]))
  checkEq ref "comm(u(b),A(a)) = false (ZII, XZI)" False (cp (at "u" [c0 "b"]) (at "A" [c0 "a"]))
  checkEq ref "comm(A(a),C(a)) = true" True (cp (at "A" [c0 "a"]) (at "C" [c0 "a"]))
  checkEq ref "comm(u(a),v) = false" False (cp (at "u" [c0 "a"]) (at "v" []))
  checkEq ref "comm(u(b),v) = true" True (cp (at "u" [c0 "b"]) (at "v" []))
  checkEq ref "unknown atoms commute" True (cp (at "zzz" []) (at "A" [c0 "a"]))
  check ref "missing n is an error" (either (const True) (const False) (parsePauliModelText "A(a) = X\n"))
  check ref "wrong length is an error" (either (const True) (const False) (parsePauliModelText "n = 2\nA(a) = X\n"))
  check ref "invalid word for an atom entry is an error" (either (const True) (const False) (parsePauliModelText "n = 1\nA(a) = Q\n"))
  -- u/v not-applicable example with the pauli backend
  let (_, pmUV) = model "n = 1\nu(a) = X\nv(a) = Z\n"
  checkEq ref "u/v example with pauli model: failing pair (u(a),v(a))"
    ([], Just (at "u" [c0 "a"], at "v" [c0 "a"])) (withReason (commPauli pmUV) pB gB)

  section "facts backend"
  let cf = mkCommFacts [(at "A" [c0 "a"], at "B" [c0 "a"]), (at "A" [c0 "a"], at "C" [c0 "a"])]
  checkEq ref "listed pair" True (cf (at "A" []) (at "B" []))
  checkEq ref "symmetric" True (cf (at "C" [c0 "x"]) (at "A" [c0 "y"]))
  checkEq ref "same predicate" True (cf (at "B" []) (at "B" [c0 "z"]))
  checkEq ref "unlisted pair" False (cf (at "B" []) (at "C" []))

  section "GHZ-style incompatible context"
  let pG = prog "XII; ZZI.\nnot ZZI.\n"
      gG = goal "?- XII."
      (_, pm3) = model "n = 3\nXII(a) = XII\nZZI(a) = ZZI\n"
  check ref "always: provable" (not (null (solveAll commAll pG gG)))
  checkEq ref "pauli: not-applicable, pair (XII, ZZI)"
    ([], Just (at "XII" [], at "ZZI" [])) (withReason (commPauli pm3) pG gG)

  (n, failed) <- readIORef ref
  putStrLn ("\n" ++ show (n - failed) ++ "/" ++ show n ++ " tests passed")
  unless (failed == 0) exitFailure
