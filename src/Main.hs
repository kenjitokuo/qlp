module Main (main) where

import QLP.Syntax
import QLP.Unify
import QLP.Search (Comm, Rule(..), Clause(..), Goal(..), QProgram, solve, solveQLP)
import QLP.Backend.Hilbert hiding (Comm)
import QLP.Parser (parseQProgramText, parseGoalText, parseCommFactsText)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Environment (getArgs)
import Data.Char (toLower)
import Text.Read (readMaybe)
import System.IO (withFile, IOMode(ReadMode), hSetEncoding, hGetContents)
import Control.Exception (try, IOException, evaluate, displayException)
import GHC.IO.Encoding (mkTextEncoding)
import Data.List (intercalate)
import System.Exit (exitFailure)

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
    (Left e, _) -> pure (Left ("Failed to read QProgram file: " ++ qprogPath ++ " (" ++ displayException e ++ ")"))
    (_, Left e) -> pure (Left ("Failed to read Goal file: " ++ goalPath ++ " (" ++ displayException e ++ ")"))
    (Right qprogText, Right goalText) -> do
      let hsQ = readMaybe qprogText :: Maybe QProgram
      let hsG = readMaybe goalText  :: Maybe Goal
      case (hsQ, hsG) of
        (Just qprog, Just goal) -> pure (Right (qprog, goal))
        _ -> case (parseQProgramText qprogText, parseGoalText goalText) of
               (Right qprog, Right goal) -> pure (Right (qprog, goal))
               (Left e1, _) -> pure (Left ("Failed to parse QProgram (Haskell readMaybe failed; text parse failed): " ++ e1))
               (_, Left e2) -> pure (Left ("Failed to parse Goal (Haskell readMaybe failed; text parse failed): " ++ e2))

readCommFacts :: FilePath -> IO (Either String [(Atom, Atom)])
readCommFacts fp = do
  e <- readFileStrictUtf8OrCP932 fp
  case e of
    Left ex -> pure (Left ("Failed to read commutativity facts file: " ++ fp ++ " (" ++ displayException ex ++ ")"))
    Right s -> pure (either (Left . ("Failed to parse commutativity facts: " ++)) Right (parseCommFactsText s))

mkCommFacts :: [(Atom, Atom)] -> Comm
mkCommFacts facts a b = any (\(x,y) -> match2 x y a b || match2 x y b a) facts where
  match2 x y a1 a2 = case unifyAtom x a1 emptySubst of { Nothing -> False; Just s1 -> case unifyAtom (applyAtom s1 y) (applyAtom s1 a2) s1 of { Nothing -> False; Just _ -> True } }

runSmoke :: FilePath -> String -> IO ()
runSmoke modelPath commMode0 = do
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

data Verdict = Provable | ProvableNonGround | NotApplicable | NoSolutions deriving (Eq, Show)

isGroundTerm :: Term -> Bool
isGroundTerm t = case t of { TVar _ -> False; TFun _ xs -> all isGroundTerm xs }

isGroundSubstFor :: [Name] -> Subst -> Bool
isGroundSubstFor xs s = all (\x -> isGroundTerm (applyTerm s (TVar x))) xs

runSolveCore :: Comm -> Int -> QProgram -> Goal -> [Subst]
runSolveCore comm maxSol qprog goal = take maxSol (solveQLP comm qprog goal emptySubst 0)

classifyHilbert :: [Name] -> Comm -> Comm -> Int -> QProgram -> Goal -> (Verdict, [Subst], [Subst])
classifyHilbert xs commHilbert commAlways maxSol qprog goal =
  let solsH = runSolveCore commHilbert maxSol qprog goal
      solsA = runSolveCore commAlways  maxSol qprog goal
  in if not (null solsH) then (if any (not . isGroundSubstFor xs) solsH then ProvableNonGround else Provable, solsH, solsA) else if not (null solsA) then (NotApplicable, [], solsA) else (NoSolutions, [], [])

runSolve :: FilePath -> String -> Maybe FilePath -> Int -> FilePath -> FilePath -> IO ()
runSolve modelPath commMode0 commFactsPath maxSol qprogPath goalPath = do
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
          let sols = runSolveCore commAlways maxSol qprog goal
          if null sols then putStrLn "(no solutions)" else mapM_ (putStrLn . showSubstFor xs) sols
        "facts" -> case commFactsPath of
          Nothing -> putStrLn "Missing --comm-facts <path> for --comm facts."
          Just fp -> do
            ef <- readCommFacts fp
            case ef of
              Left msg -> putStrLn msg
              Right facts -> do
                let commFacts = mkCommFacts facts
                let sols = runSolveCore commFacts maxSol qprog goal
                if null sols then putStrLn "(no solutions)" else mapM_ (putStrLn . showSubstFor xs) sols
        _ -> do
          let (v, solsH, solsA) = classifyHilbert xs commHilbert commAlways maxSol qprog goal
          case v of
            Provable -> do { putStrLn "(provable)"; mapM_ (putStrLn . showSubstFor xs) solsH }
            ProvableNonGround -> do { putStrLn "(provable-nonground)"; mapM_ (putStrLn . showSubstFor xs) solsH }
            NotApplicable -> do { putStrLn "(not-applicable)"; if null solsA then pure () else mapM_ (putStrLn . showSubstFor xs) solsA }
            NoSolutions -> putStrLn "(no solutions)"

runCompare :: FilePath -> Int -> FilePath -> FilePath -> IO ()
runCompare modelPath maxSol qprogPath goalPath = do
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
      let (v, solsH, solsA) = classifyHilbert xs commHilbert commAlways maxSol qprog goal
      case v of
        Provable -> do { putStrLn "(provable)"; mapM_ (putStrLn . showSubstFor xs) solsH }
        ProvableNonGround -> do { putStrLn "(provable-nonground)"; mapM_ (putStrLn . showSubstFor xs) solsH }
        NotApplicable -> do { putStrLn "(not-applicable)"; if null solsA then pure () else mapM_ (putStrLn . showSubstFor xs) solsA }
        NoSolutions -> putStrLn "(no solutions)"

escapeJson :: String -> String
escapeJson = concatMap esc where esc '"' = "\\\""; esc '\\' = "\\\\"; esc '\n' = "\\n"; esc '\r' = "\\r"; esc '\t' = "\\t"; esc c = [c]

jstr :: String -> String
jstr s = "\"" ++ escapeJson s ++ "\""

jarrStr :: [String] -> String
jarrStr xs = "[" ++ intercalate "," (map jstr xs) ++ "]"

verdictText :: Verdict -> String
verdictText v = case v of { Provable -> "provable"; ProvableNonGround -> "provable-nonground"; NotApplicable -> "not-applicable"; NoSolutions -> "no-solutions" }

compareJsonLine :: FilePath -> FilePath -> FilePath -> Int -> [Name] -> Verdict -> [Subst] -> [Subst] -> String
compareJsonLine modelPath qprogPath goalPath maxSol xs v solsH solsA =
  let sh = map (showSubstFor xs) solsH
      sa = map (showSubstFor xs) solsA
  in "{" ++ intercalate "," [ "\"model\":" ++ jstr modelPath, "\"qprog\":" ++ jstr qprogPath, "\"goal\":" ++ jstr goalPath, "\"max_sol\":" ++ show maxSol, "\"vars\":" ++ jarrStr xs, "\"verdict\":" ++ jstr (verdictText v), "\"sols_hilbert\":" ++ jarrStr sh, "\"sols_always\":" ++ jarrStr sa ] ++ "}"

runCompareJsonl :: FilePath -> Int -> FilePath -> FilePath -> FilePath -> IO ()
runCompareJsonl modelPath maxSol outPath qprogPath goalPath = do
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
      let (v, solsH, solsA) = classifyHilbert xs commHilbert commAlways maxSol qprog goal
      let line = compareJsonLine modelPath qprogPath goalPath maxSol xs v solsH solsA
      appendFile outPath (line ++ "\n")
      case v of
        Provable -> do { putStrLn "(provable)"; mapM_ (putStrLn . showSubstFor xs) solsH }
        ProvableNonGround -> do { putStrLn "(provable-nonground)"; mapM_ (putStrLn . showSubstFor xs) solsH }
        NotApplicable -> do { putStrLn "(not-applicable)"; if null solsA then pure () else mapM_ (putStrLn . showSubstFor xs) solsA }
        NoSolutions -> putStrLn "(no solutions)"

sampleQProg :: QProgram
sampleQProg = [ Clause {negAtoms = [Atom "P" [TVar "X"]], posAtoms = [Atom "Q" [TVar "X"]]}, Clause {negAtoms = [], posAtoms = [Atom "P" [TFun "a" []]]}, Clause {negAtoms = [], posAtoms = [Atom "R" [TFun "a" []]]} ]

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

extractCommFactsArg :: [String] -> (Maybe FilePath, [String])
extractCommFactsArg [] = (Nothing, [])
extractCommFactsArg ["--comm-facts"] = (Nothing, ["--comm-facts"])
extractCommFactsArg ("--comm-facts":fp:xs) = (Just fp, xs)
extractCommFactsArg (x:xs) = let (fp, ys) = extractCommFactsArg xs in (fp, x:ys)

extractMaxSolArg :: [String] -> (Int, [String])
extractMaxSolArg [] = (20, [])
extractMaxSolArg ["--max-sol"] = (20, ["--max-sol"])
extractMaxSolArg ("--max-sol":n:xs) = case readMaybe n of { Just k -> (k, xs); Nothing -> (20, ("--max-sol":n:xs)) }
extractMaxSolArg (x:xs) = let (k, ys) = extractMaxSolArg xs in (k, x:ys)

main :: IO ()
main = do
  args <- getArgs
  let (modelPath, args1) = extractModelArg args
  let (commMode, args2)  = extractCommArg args1
  let (commFacts, args3) = extractCommFactsArg args2
  let (maxSol, rest)     = extractMaxSolArg args3
  case rest of
    ["--run"] -> runSmoke modelPath commMode
    ("--emit-sample":qprogPath:goalPath:[]) -> runEmitSample qprogPath goalPath
    ("--solve":qprogPath:goalPath:[]) -> runSolve modelPath commMode commFacts maxSol qprogPath goalPath
    ("--compare":qprogPath:goalPath:[]) -> runCompare modelPath maxSol qprogPath goalPath
    ("--compare-jsonl":outPath:qprogPath:goalPath:[]) -> runCompareJsonl modelPath maxSol outPath qprogPath goalPath
    _ -> do
      putStrLn "Unknown/invalid arguments."
      putStrLn ("args = " ++ show args)
      putStrLn "Expected one of:"
      putStrLn "  --run"
      putStrLn "  --emit-sample <qprogPath> <goalPath>"
      putStrLn "  --solve <qprogPath> <goalPath>"
      putStrLn "  --compare <qprogPath> <goalPath>"
      putStrLn "  --compare-jsonl <outPath> <qprogPath> <goalPath>"
      putStrLn "Options:"
      putStrLn "  --model <path> --comm <hilbert|always|facts> --comm-facts <path> --max-sol <n>"
      exitFailure
