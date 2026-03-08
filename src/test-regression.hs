import qualified Data.Map.Strict as M
import QLP.Syntax
import QLP.Unify
import QLP.Search

v :: String -> Term
v = TVar

f :: String -> [Term] -> Term
f = TFun

a :: String -> [Term] -> Atom
a = Atom

s0 :: Subst
s0 = M.empty

commEqPred :: Comm
commEqPred (Atom p _) (Atom q _) = p == q

main :: IO ()
main = do
  let prog =
        [ Clause [a "n" [v "X"]] [a "p" [v "X"]]
        , Clause [] [a "n" [f "a" []]]
        , Clause [] [a "z" [f "a" []]]
        ]
      goal =
        Goal [a "p" [f "a" []], a "z" [f "a" []]] []
  print (solveQLPWithDebug True commEqPred prog goal s0 0)