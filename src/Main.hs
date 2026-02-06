module Main (main) where

import QLP.Syntax
import QLP.Unify
import QLP.Search (Comm, Rule(..), Clause(..), Goal(..), QProgram, solve, solveQLPWithDebug)
import QLP.Backend.Hilbert hiding (Comm)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Environment (getArgs)
import Data.Char (toLower)
import Text.Read (readMaybe)
import System.IO (withFile, IOMode(ReadMode), hSetEncoding, hGetContents)
import Control.Exception (try, IOException, evaluate)
import GHC.IO.Encoding (mkTextEncoding)
import Data.List (intercalate)

showSubst :: Subst -> String
showSubst s = let items = [x ++ " -> " ++ show t | (x,t) <- M.toList s] in "{" ++ unwords items ++ "}"

showSubstFor :: [Name] -> Subst -> String
showSubstFor xs s = let items = [x ++ " -> " ++ show (applyTerm s (TVar x)) | x <- xs] in "{" ++ unwords items ++ "}"

varsTerm0 :: Term -> S.Set Name
varsTerm0 t = case t of { TVar x -> S.singleton x; TFun _ xs -> S.unions (map varsTerm0 xs) }

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
  case e1 of { Right s -> pure (Right s); Left _ -> readFileStrictEnc fp "CP932" }

readQProgGoal :: FilePath -> FilePath -> IO (Either String (QProgram, Goal))
readQProgGoal qprogPath goalPath = do
  eqprog <- readFileStrictUtf8OrCP932 qprogPath
  egoal  <- readFileStrictUtf8OrCP932 goalPath
  case (eqprog, egoal) of
    (Left _, _) -> pure (Left "Failed to read QProgram file.")
    (_, Left _) -> pure (Left "Failed to read Goal file.")
    (Right qprogText, Right goalText) -> do
      let mqprog = readMaybe qprogText :: Maybe QProgram
      let mgoal  = readMaybe goalText  :: Maybe Goal
      case (mqprog, mgoal) of
        (Nothing, _) -> pure (Left "Failed to parse QProgram file (expected a Haskell value of type QProgram).")
        (_, Nothing) -> pure (Left "Failed to parse Goal file (expected a Haskell value of type Goal).")
        (Just qprog, Just goal) -> pure (Right (qprog, goal))

runSmoke :: FilePath -> String -> Bool -> IO ()
runSmoke modelPath commMode0 dbg = do
  model <- loadModelOrDefault modelPath
  let commMode = map toLower commMode0
  let comm :: Comm
      comm = case commMode of { "always" -> \_ _ -> True; _ -> commutes model }

  putStrLn "QLP unify smoke test"
  let t1 = TFun "f" [TVar "X", TFun "a" []]
  let t2 = TFun "f" [TFun "b" [], TVar "Y"]
  case unifyTerm t1 t2 emptySubst of { Nothing -> putStrLn "unifyTerm: fail"; Just s -> putStrLn ("unifyTerm: ok " ++ showSubst s) }

  let a1 = Atom "P" [TVar "X", TFun "g" [TVar "X"]]
  let a2 = Atom "P" [TFun "c" [], TVar "Z"]
  case unifyAtom a1 a2 emptySubst of { Nothing -> putStrLn "unifyAtom: fail"; Just s -> putStrLn ("unifyAtom: ok " ++ showSubst s) }

  let cyc1 = TVar "X"
  let cyc2 = TFun "f" [TVar "X"]
  case unifyTerm cyc1 cyc2 emptySubst of { Nothing -> putStrLn "occurs-check: ok (rejected)"; Just s -> putStrLn ("occurs-check: unexpected " ++ showSubst s) }

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
  let solsQ = take 3 (solveQLPWithDebug dbg comm qprog gQ emptySubst 0)
  if null solsQ then putStrLn "(no solutions)" else mapM_ (putStrLn . showSubstFor ["Y"]) solsQ

  putStrLn "QLP: goal R(Y)"
  let gR = Goal [Atom "R" [TVar "Y"]] []
  let solsR = take 3 (solveQLPWithDebug dbg comm qprog gR emptySubst 0)
  if null solsR then putStrLn "(no solutions)" else mapM_ (putStrLn . showSubstFor ["Y"]) solsR

  putStrLn "QLP: goal Q(Y) and R(Y)"
  let gQR = Goal [Atom "Q" [TVar "Y"], Atom "R" [TVar "Y"]] []
  let solsQR = take 3 (solveQLPWithDebug dbg comm qprog gQR emptySubst 0)
  if null solsQR then putStrLn "(no solutions)" else mapM_ (putStrLn . showSubstFor ["Y"]) solsQR

data Verdict = Provable | ProvableNonground | NotApplicable | NoSolutions deriving (Eq, Show)

isGroundTerm :: Term -> Bool
isGroundTerm t = case t of { TVar _ -> False; TFun _ xs -> all isGroundTerm xs }

isGroundFor :: [Name] -> Subst -> Bool
isGroundFor xs s = all (\x -> isGroundTerm (applyTerm s (TVar x))) xs

runSolveCore :: Bool -> Comm -> Int -> QProgram -> Goal -> [Subst]
runSolveCore dbg comm maxSol qprog goal = take maxSol (solveQLPWithDebug dbg comm qprog goal emptySubst 0)

classifyHilbert :: Bool -> [Name] -> Comm -> Comm -> Int -> QProgram -> Goal -> (Verdict, [Subst], [Subst])
classifyHilbert dbg xs commHilbert commAlways maxSol qprog goal =
  let solsH = runSolveCore dbg commHilbert maxSol qprog goal
      solsA = runSolveCore dbg commAlways  maxSol qprog goal
  in if not (null solsH)
       then if any (isGroundFor xs) solsH then (Provable, solsH, solsA) else (ProvableNonground, solsH, solsA)
       else if not (null solsA)
              then if any (isGroundFor xs) solsA then (NotApplicable, [], solsA) else (ProvableNonground, [], solsA)
              else (NoSolutions, [], [])

runSolve :: FilePath -> String -> Int -> Bool -> FilePath -> FilePath -> IO ()
runSolve modelPath commMode0 maxSol dbg qprogPath goalPath = do
  model <- loadModelOrDefault modelPath
  let commMode = map toLower commMode0
  let commHilbert :: Comm
      commHilbert = commutes model
  let commAlways :: Comm
      commAlways = \_ _ -> True
  e <- readQProgGoal qprogPath goalPath
  case e of
    Left msg -> putStrLn msg
    Right (qprog, goal) -> do
      let xs = goalVars goal
      case commMode of
        "always" -> do
          let sols = runSolveCore dbg commAlways maxSol qprog goal
          if null sols then putStrLn "(no solutions)" else mapM_ (putStrLn . showSubstFor xs) sols
        _ -> do
          let (v, solsH, _solsA) = classifyHilbert dbg xs commHilbert commAlways maxSol qprog goal
          case v of
            Provable -> mapM_ (putStrLn . showSubstFor xs) solsH
            ProvableNonground -> mapM_ (putStrLn . showSubstFor xs) solsH
            NotApplicable -> putStrLn "(not-applicable)"
            NoSolutions -> putStrLn "(no solutions)"

runCompare :: FilePath -> Int -> Bool -> FilePath -> FilePath -> IO ()
runCompare modelPath maxSol dbg qprogPath goalPath = do
  model <- loadModelOrDefault modelPath
  let commHilbert :: Comm
      commHilbert = commutes model
  let commAlways :: Comm
      commAlways = \_ _ -> True
  e <- readQProgGoal qprogPath goalPath
  case e of
    Left msg -> putStrLn msg
    Right (qprog, goal) -> do
      let xs = goalVars goal
      let (v, solsH, solsA) = classifyHilbert dbg xs commHilbert commAlways maxSol qprog goal
      case v of
        Provable -> do { putStrLn "(provable)"; mapM_ (putStrLn . showSubstFor xs) solsH }
        ProvableNonground -> do { putStrLn "(provable-nonground)"; if null solsH then pure () else mapM_ (putStrLn . showSubstFor xs) solsH }
        NotApplicable -> do { putStrLn "(not-applicable)"; if null solsA then pure () else mapM_ (putStrLn . showSubstFor xs) solsA }
        NoSolutions -> putStrLn "(no solutions)"

escapeJson :: String -> String
escapeJson = concatMap esc where esc '"' = "\\\""; esc '\\' = "\\\\"; esc '\n' = "\\n"; esc '\r' = "\\r"; esc '\t' = "\\t"; esc c = [c]

jstr :: String -> String
jstr s = "\"" ++ escapeJson s ++ "\""

jarrStr :: [String] -> String
jarrStr xs = "[" ++ intercalate "," (map jstr xs) ++ "]"

verdictText :: Verdict -> String
verdictText v = case v of { Provable -> "provable"; ProvableNonground -> "provable-nonground"; NotApplicable -> "not-applicable"; NoSolutions -> "no-solutions" }

compareJsonLine :: FilePath -> FilePath -> FilePath -> Int -> [Name] -> Verdict -> [Subst] -> [Subst] -> String
compareJsonLine modelPath qprogPath goalPath maxSol xs v solsH solsA =
  let sh = map (showSubstFor xs) solsH
      sa = map (showSubstFor xs) solsA
  in "{" ++ intercalate "," [ "\"model\":" ++ jstr modelPath, "\"qprog\":" ++ jstr qprogPath, "\"goal\":" ++ jstr goalPath, "\"max_sol\":" ++ show maxSol, "\"vars\":" ++ jarrStr xs, "\"verdict\":" ++ jstr (verdictText v), "\"sols_hilbert\":" ++ jarrStr sh, "\"sols_always\":" ++ jarrStr sa ] ++ "}"

runCompareJsonl :: FilePath -> Int -> Bool -> FilePath -> FilePath -> FilePath -> IO ()
runCompareJsonl modelPath maxSol dbg outPath qprogPath goalPath = do
  model <- loadModelOrDefault modelPath
  let commHilbert :: Comm
      commHilbert = commutes model
  let commAlways :: Comm
      commAlways = \_ _ -> True
  e <- readQProgGoal qprogPath goalPath
  case e of
    Left msg -> putStrLn msg
    Right (qprog, goal) -> do
      let xs = goalVars goal
      let (v, solsH, solsA) = classifyHilbert dbg xs commHilbert commAlways maxSol qprog goal
      let line = compareJsonLine modelPath qprogPath goalPath maxSol xs v solsH solsA
      appendFile outPath (line ++ "\n")
      case v of
        Provable -> do { putStrLn "(provable)"; mapM_ (putStrLn . showSubstFor xs) solsH }
        ProvableNonground -> do { putStrLn "(provable-nonground)"; if null solsH then pure () else mapM_ (putStrLn . showSubstFor xs) solsH }
        NotApplicable -> do { putStrLn "(not-applicable)"; if null solsA then pure () else mapM_ (putStrLn . showSubstFor xs) solsA }
        NoSolutions -> putStrLn "(no solutions)"

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
extractModelArg (x:xs) = let (fp, ys) = extractModelArg xs in (fp, x:ys)

extractCommArg :: [String] -> (String, [String])
extractCommArg [] = ("hilbert", [])
extractCommArg ["--comm"] = ("hilbert", ["--comm"])
extractCommArg ("--comm":mode:xs) = (mode, xs)
extractCommArg (x:xs) = let (mode, ys) = extractCommArg xs in (mode, x:ys)

extractMaxSolArg :: [String] -> (Int, [String])
extractMaxSolArg [] = (20, [])
extractMaxSolArg ["--max-sol"] = (20, ["--max-sol"])
extractMaxSolArg ("--max-sol":n:xs) = case readMaybe n of { Just k -> (k, xs); Nothing -> (20, ("--max-sol":n:xs)) }
extractMaxSolArg (x:xs) = let (k, ys) = extractMaxSolArg xs in (k, x:ys)

extractDebugCommFlag :: [String] -> (Bool, [String])
extractDebugCommFlag [] = (False, [])
extractDebugCommFlag ("--debug-comm":xs) = (True, xs)
extractDebugCommFlag (x:xs) = let (b, ys) = extractDebugCommFlag xs in (b, x:ys)

main :: IO ()
main = do
  args <- getArgs
  let (dbg, args0)       = extractDebugCommFlag args
  let (modelPath, args1) = extractModelArg args0
  let (commMode, args2)  = extractCommArg args1
  let (maxSol, rest)     = extractMaxSolArg args2
  case rest of
    ["--run"] -> runSmoke modelPath commMode dbg
    ("--emit-sample":qprogPath:goalPath:[]) -> runEmitSample qprogPath goalPath
    ("--solve":qprogPath:goalPath:[]) -> runSolve modelPath commMode maxSol dbg qprogPath goalPath
    ("--compare":qprogPath:goalPath:[]) -> runCompare modelPath maxSol dbg qprogPath goalPath
    ("--compare-jsonl":outPath:qprogPath:goalPath:[]) -> runCompareJsonl modelPath maxSol dbg outPath qprogPath goalPath
    _ -> runSmoke modelPath commMode dbg
