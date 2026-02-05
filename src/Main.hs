module Main (main) where

import QLP.Syntax
import QLP.Unify
import QLP.Search (Rule(..), Clause(..), Goal(..), QProgram, solve, solveQLP)
import QLP.Backend.Hilbert
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Environment (getArgs)
import Data.Char (toLower)
import Text.Read (readMaybe)
import System.IO (withFile, IOMode(ReadMode), hSetEncoding, hGetContents)
import Control.Exception (try, IOException, evaluate)
import GHC.IO.Encoding (mkTextEncoding)

showSubst :: Subst -> String
showSubst s =
  let items = [x ++ " -> " ++ show t | (x,t) <- M.toList s]
  in "{" ++ unwords items ++ "}"

showSubstFor :: [Name] -> Subst -> String
showSubstFor xs s =
  let items = [x ++ " -> " ++ show (applyTerm s (TVar x)) | x <- xs]
  in "{" ++ unwords items ++ "}"


varsTerm0 :: Term -> S.Set Name
varsTerm0 t = case t of
  TVar x -> S.singleton x
  TFun _ xs -> S.unions (map varsTerm0 xs)

varsAtom0 :: Atom -> S.Set Name
varsAtom0 (Atom _ xs) = S.unions (map varsTerm0 xs)

goalVars :: Goal -> [Name]
goalVars (Goal ps ns) = S.toList (S.unions (map varsAtom0 (ps ++ ns)))

stripUtf8BOM :: String -> String
stripUtf8BOM ('\xFEFF':cs) = cs
stripUtf8BOM cs = cs

readFileStrictEnc :: FilePath -> String -> IO (Either IOException String)
readFileStrictEnc fp encName = try $ do
  enc <- mkTextEncoding encName
  withFile fp ReadMode $ \h -> do
    hSetEncoding h enc
    s <- hGetContents h
    _ <- evaluate (length s)
    pure (stripUtf8BOM s)

readFileStrictUtf8OrCP932 :: FilePath -> IO (Either IOException String)
readFileStrictUtf8OrCP932 fp = do
  e1 <- readFileStrictEnc fp "UTF-8"
  case e1 of
    Right s -> pure (Right s)
    Left _  -> readFileStrictEnc fp "CP932"


runSmoke :: FilePath -> String -> IO ()
runSmoke modelPath commMode0 = do
  model <- loadModelOrDefault modelPath
  let commMode = map toLower commMode0
  let comm :: Atom -> Atom -> Bool
      comm =
        case commMode of
          "always" -> \_ _ -> True
          _        -> commutes model

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

  let parentAB = Rule (Atom "parent" [TFun "alice" [], TFun "bob" []]) []
  let parentBC = Rule (Atom "parent" [TFun "bob" [], TFun "carol" []]) []
  let anc1 = Rule (Atom "ancestor" [TVar "X", TVar "Y"]) [Atom "parent" [TVar "X", TVar "Y"]]
  let anc2 = Rule (Atom "ancestor" [TVar "X", TVar "Y"]) [Atom "parent" [TVar "X", TVar "Z"], Atom "ancestor" [TVar "Z", TVar "Y"]]
  let prog = [parentAB, parentBC, anc1, anc2]

  let q = [Atom "ancestor" [TFun "alice" [], TVar "Y"]]
  let sols = take 5 (solve prog q emptySubst 0)
  mapM_ (putStrLn . showSubstFor ["Y"]) sols

  putStrLn ("Search smoke test (QLP, comm=" ++ (if commMode == "always" then "always" else "hilbert") ++ ")")

  let c1 = Clause [Atom "P" [TVar "X"]] [Atom "Q" [TVar "X"]]
  let c2 = Clause [] [Atom "P" [TFun "a" []]]
  let c3 = Clause [] [Atom "R" [TFun "a" []]]
  let qprog = [c1, c2, c3]

  putStrLn "QLP: goal Q(Y)"
  let gQ = Goal [Atom "Q" [TVar "Y"]] []
  let solsQ = take 3 (solveQLP comm qprog gQ emptySubst 0)
  if null solsQ then putStrLn "(no solutions)" else mapM_ (putStrLn . showSubstFor ["Y"]) solsQ

  putStrLn "QLP: goal R(Y)"
  let gR = Goal [Atom "R" [TVar "Y"]] []
  let solsR = take 3 (solveQLP comm qprog gR emptySubst 0)
  if null solsR then putStrLn "(no solutions)" else mapM_ (putStrLn . showSubstFor ["Y"]) solsR

  putStrLn "QLP: goal Q(Y) and R(Y)"
  let gQR = Goal [Atom "Q" [TVar "Y"], Atom "R" [TVar "Y"]] []
  let solsQR = take 3 (solveQLP comm qprog gQR emptySubst 0)
  if null solsQR then putStrLn "(no solutions)" else mapM_ (putStrLn . showSubstFor ["Y"]) solsQR

  putStrLn ("Commutativity quick check (" ++ (if commMode == "always" then "always" else "hilbert") ++ ")")
  print (comm (Atom "P" []) (Atom "Q" []))
  print (comm (Atom "P" []) (Atom "R" []))
  print (comm (Atom "Q" []) (Atom "R" []))

runSolve :: FilePath -> String -> FilePath -> FilePath -> IO ()
runSolve modelPath commMode0 qprogPath goalPath = do
  model <- loadModelOrDefault modelPath
  let commMode = map toLower commMode0
  let comm :: Atom -> Atom -> Bool
      comm =
        case commMode of
          "always" -> \_ _ -> True
          _        -> commutes model

  eqprog <- readFileStrictUtf8OrCP932 qprogPath
  egoal  <- readFileStrictUtf8OrCP932 goalPath
  case (eqprog, egoal) of
    (Left _, _) -> putStrLn "Failed to read QProgram file."
    (_, Left _) -> putStrLn "Failed to read Goal file."
    (Right qprogText, Right goalText) -> do
      let mqprog = readMaybe qprogText :: Maybe QProgram
      let mgoal  = readMaybe goalText  :: Maybe Goal
      case (mqprog, mgoal) of
        (Nothing, _) -> putStrLn "Failed to parse QProgram file (expected a Haskell value of type QProgram)."
        (_, Nothing) -> putStrLn "Failed to parse Goal file (expected a Haskell value of type Goal)."
        (Just qprog, Just goal) -> do
          let xs = goalVars goal
          let sols = take 20 (solveQLP comm qprog goal emptySubst 0)
          if null sols then putStrLn "(no solutions)" else mapM_ (putStrLn . showSubstFor xs) sols


  qprogText0 <- readFile qprogPath
  goalText0  <- readFile goalPath

  let qprogText = stripUtf8BOM qprogText0
  let goalText  = stripUtf8BOM goalText0

  let mqprog = readMaybe qprogText :: Maybe QProgram
  let mgoal  = readMaybe goalText  :: Maybe Goal


  case (mqprog, mgoal) of
    (Nothing, _) -> putStrLn "Failed to parse QProgram file (expected a Haskell value of type QProgram)."
    (_, Nothing) -> putStrLn "Failed to parse Goal file (expected a Haskell value of type Goal)."
    (Just qprog, Just goal) -> do
      let xs = goalVars goal
      let sols = take 20 (solveQLP comm qprog goal emptySubst 0)
      if null sols then putStrLn "(no solutions)" else mapM_ (putStrLn . showSubstFor xs) sols

sampleQProg :: QProgram
sampleQProg =
  [ Clause {negAtoms = [Atom "P" [TVar "X"]], posAtoms = [Atom "Q" [TVar "X"]]}
  , Clause {negAtoms = [], posAtoms = [Atom "P" [TFun "a" []]]}
  , Clause {negAtoms = [], posAtoms = [Atom "R" [TFun "a" []]]}
  ]

sampleGoal :: Goal
sampleGoal = Goal {wantPos = [Atom "Q" [TVar "Y"], Atom "R" [TVar "Y"]], wantNeg = []}

runEmitSample :: FilePath -> FilePath -> IO ()
runEmitSample qprogPath goalPath = do
  writeFile qprogPath (show sampleQProg ++ "\n")
  writeFile goalPath (show sampleGoal ++ "\n")
  putStrLn ("Wrote: " ++ qprogPath)
  putStrLn ("Wrote: " ++ goalPath)

extractModelArg :: [String] -> (FilePath, [String])
extractModelArg [] = ("hilbert.conf", [])
extractModelArg ["--model"] = ("hilbert.conf", ["--model"])
extractModelArg ("--model":fp:xs) = (fp, xs)
extractModelArg (x:xs) =
  let (fp, ys) = extractModelArg xs
  in (fp, x:ys)

extractCommArg :: [String] -> (String, [String])
extractCommArg [] = ("hilbert", [])
extractCommArg ["--comm"] = ("hilbert", ["--comm"])
extractCommArg ("--comm":mode:xs) = (mode, xs)
extractCommArg (x:xs) =
  let (mode, ys) = extractCommArg xs
  in (mode, x:ys)

main :: IO ()
main = do
  args <- getArgs
  let (modelPath, args1) = extractModelArg args
  let (commMode, rest) = extractCommArg args1
  case rest of
    ["--run"] -> runSmoke modelPath commMode
    ("--emit-sample":qprogPath:goalPath:[]) -> runEmitSample qprogPath goalPath
    ("--solve":qprogPath:goalPath:[]) -> runSolve modelPath commMode qprogPath goalPath
    _ -> runSmoke modelPath commMode
