module Main where

import QLP.Syntax
import QLP.Unify
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  let t = TFun "f" [TVar "X", TFun "a" []]
  let a = Atom "P" [t]
  putStrLn "QLP scaffold OK"
  print t
  print a
  print (M.size emptySubst)
