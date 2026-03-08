import qualified Data.Map.Strict as M
import QLP.Syntax
import QLP.Search
import QLP.Unify

v :: String -> Term
v = TVar

f :: String -> [Term] -> Term
f = TFun

a :: String -> [Term] -> Atom
a = Atom

s0 :: Subst
s0 = M.empty

commAll :: Comm
commAll _ _ = True

commEqPred :: Comm
commEqPred (Atom p _) (Atom q _) = p == q

runCase :: String -> Comm -> QProgram -> Goal -> IO ()
runCase name comm prog goal = do
  putStrLn ("=== " ++ name ++ " ===")
  let ans = solveQLPWithDebug True comm prog goal s0 0
  putStrLn ("answers = " ++ show ans)
  putStrLn ""

main :: IO ()
main = do
  -- BC(I): positive goal, matched clause has remaining positive literals
  -- Clause: n(X) ; p(X), q(X)
  -- Goal:   p(a)
  -- Expected after backchaining:
  --   newPos = [n(a)], newNeg = [q(a)]
  -- Side condition should be checked only between p(a) and [n(a), q(a)].
  let progBC1 =
        [ Clause [a "n" [v "X"]] [a "p" [v "X"], a "q" [v "X"]] ]
      goalBC1 = Goal [a "p" [f "a" []]] []
  runCase "BC(I) with commAll" commAll progBC1 goalBC1
  runCase "BC(I) with commEqPred" commEqPred progBC1 goalBC1

  -- BC(III): positive goal, matched clause has no remaining positive literals
  -- Clause: n(X) ; p(X)
  -- Goal:   p(a)
  -- Expected:
  --   no side condition is added
  let progBC3 =
        [ Clause [a "n" [v "X"]] [a "p" [v "X"]] ]
      goalBC3 = Goal [a "p" [f "a" []]] []
  runCase "BC(III) with commAll" commAll progBC3 goalBC3
  runCase "BC(III) with commEqPred" commEqPred progBC3 goalBC3

  -- BC(II): negative goal, matched clause has remaining negative literals
  -- Clause: s(X), t(X) ; p(X)
  -- Goal:   ~s(a)
  -- Expected after backchaining:
  --   newPos = [p(a)], newNeg = [t(a)]
  -- Side condition should be checked only between s(a) and [t(a), p(a)].
  let progBC2 =
        [ Clause [a "s" [v "X"], a "t" [v "X"]] [a "p" [v "X"]] ]
      goalBC2 = Goal [] [a "s" [f "a" []]]
  runCase "BC(II) with commAll" commAll progBC2 goalBC2
  runCase "BC(II) with commEqPred" commEqPred progBC2 goalBC2

  -- BC(IV): negative goal, matched clause has no remaining negative literals
  -- Clause: s(X) ; p(X)
  -- Goal:   ~s(a)
  -- Expected:
  --   no side condition is added
  let progBC4 =
        [ Clause [a "s" [v "X"]] [a "p" [v "X"]] ]
      goalBC4 = Goal [] [a "s" [f "a" []]]
  runCase "BC(IV) with commAll" commAll progBC4 goalBC4
  runCase "BC(IV) with commEqPred" commEqPred progBC4 goalBC4