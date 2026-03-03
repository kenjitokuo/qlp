module Main (main) where

import QLP.Syntax
import QLP.Unify
import QLP.Search (Comm, Rule(..), Clause(..), Goal(..), QProgram, solve, solveQLPWithDebug)
import QLP.Backend.Hilbert hiding (Comm)
import QLP.Parser (parseQProgramText, parseGoalText, parseCommFactsText)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Environment (getArgs)
import Data.Char (toLower, isSpace, isUpper)
import Text.Read (readMaybe)
import System.IO (withFile, IOMode(ReadMode), hSetEncoding, hGetContents)
import Control.Exception (try, IOException, evaluate, displayException)
import GHC.IO.Encoding (mkTextEncoding)
import Data.List (intercalate, dropWhileEnd)
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


-- Normalize Atom for commutativity backends: by default, ignore arguments and keep only the predicate symbol.
predOnly :: Atom -> Atom
predOnly (Atom p _) = Atom p []

readCommFacts :: FilePath -> IO (Either String [(Atom, Atom)])
readCommFacts fp = do
  e <- readFileStrictUtf8OrCP932 fp
  case e of
    Left ex -> pure (Left ("Failed to read commutativity facts file: " ++ fp ++ " (" ++ displayException ex ++ ")"))
    Right s ->
      let normalizeFacts = map (\(a,b) -> (predOnly a, predOnly b))
      in pure (either (Left . ("Failed to parse commutativity facts: " ++)) (Right . normalizeFacts) (parseCommFactsText s))

mkCommFacts :: [(Atom, Atom)] -> Comm
mkCommFacts facts0 a b =
  let facts = map (\(x,y) -> (predOnly x, predOnly y)) facts0
      Atom pa _ = predOnly a
      Atom pb _ = predOnly b
      okPair (Atom px _) (Atom py _) = (px == pa && py == pb) || (px == pb && py == pa)
  in pa == pb || any (\(x,y) -> okPair x y) facts

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

stripComment :: String -> String
stripComment s = takeWhile (/= '#') s

takeName :: String -> String
takeName s =
  let t = trim s
  in takeWhile (\c -> c /= '(' && not (isSpace c)) t

isPauliChar :: Char -> Bool
isPauliChar c = c == 'I' || c == 'X' || c == 'Y' || c == 'Z'

normalizePauliWord :: String -> String
normalizePauliWord = filter (not . isSpace)

commutesPauli :: String -> String -> Either String Bool
commutesPauli w1 w2 =
  if length w1 /= length w2 then Left ("Pauli words have different lengths: " ++ show (length w1, length w2))
  else if any (not . isPauliChar) w1 || any (not . isPauliChar) w2 then Left ("Pauli words must be over {I,X,Y,Z}: " ++ show (w1, w2))
  else
    let anticomm (a,b) = a /= 'I' && b /= 'I' && a /= b
        k = length (filter anticomm (zip w1 w2))
    in Right (k `mod` 2 == 0)

-- Pauli モード用：混在 conf でも壊れないように「P(...) = <word>」だけ拾う。
-- それ以外の key=value（dim=..., eps=..., Q=... など）は無視する。
parsePauliModelText :: String -> Either String (Int, M.Map String String)
parsePauliModelText s =
  let ls  = [trim (stripComment l) | l <- lines s]
      ls1 = filter (not . null) ls
      step (mn, mp) l =
        case break (=='=') l of
          (lhs,'=':rhs0) ->
            let lhs1 = trim lhs
                rhs1 = trim rhs0
                lhsLower = map toLower lhs1
            in if lhsLower == "n" then
                 case readMaybe rhs1 of
                   Nothing -> Left ("Failed to parse n in model: " ++ l)
                   Just k  -> Right (Just k, mp)
               else if '(' `elem` lhs1 then
                 let nm0 = takeName lhs1
                     nm  = trim nm0
                     w   = normalizePauliWord rhs1
                 in if null nm then Right (mn, mp)
                   else if not (isUpper (case nm of { (c:_) -> c; [] -> 'a' })) then Right (mn, mp)
                    else Right (mn, M.insert nm w mp)
               else
                 Right (mn, mp)
          _ -> Right (mn, mp)  -- '=' がない行は無視
      go (mn, mp) [] = case mn of { Nothing -> Left "Missing n = <int> in model"; Just k -> Right (k, mp) }
      go st (l:rest) = do { st' <- step st l; go st' rest }
  in do
    (n, mp0) <- go (Nothing, M.empty) ls1
    let badLen  = [(k,w) | (k,w) <- M.toList mp0, length w /= n]
    let badChar = [(k,w) | (k,w) <- M.toList mp0, any (not . isPauliChar) w]
    if not (null badLen) then Left ("Pauli word length mismatch (expected n=" ++ show n ++ "): " ++ show badLen)
    else if not (null badChar) then Left ("Pauli word has invalid chars (allowed {I,X,Y,Z}): " ++ show badChar)
    else Right (n, mp0)

mkCommPauliFromModelFile :: FilePath -> IO (Either String Comm)
mkCommPauliFromModelFile fp = do
  e <- readFileStrictUtf8OrCP932 fp
  case e of
    Left ex -> pure (Left ("Failed to read model file: " ++ fp ++ " (" ++ displayException ex ++ ")"))
    Right txt ->
      case parsePauliModelText txt of
        Left msg -> pure (Left msg)
        Right (_n, mp) ->
          let comm :: Comm
              comm (Atom p _) (Atom q _) =
                case (M.lookup p mp, M.lookup q mp) of
                  (Just w1, Just w2) ->
                    case commutesPauli w1 w2 of
                      Right b  -> b
                      Left msg -> error ("commutesPauli: " ++ msg)
                  _ -> True  -- 未定義は commutes に倒す（Hilbert の Unknown と同じ方針）
          in pure (Right comm)

runSmoke :: Bool -> FilePath -> String -> IO ()
runSmoke dbg modelPath commMode0 = do
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

  putStrLn ("Search smoke test (QLP, comm=" ++ (if commMode == "always" then "always" else "hilbert") ++ ", debug=" ++ show dbg ++ ")")

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

  putStrLn ("Commutativity quick check (" ++ (if commMode == "always" then "always" else "hilbert") ++ ")")
  print (comm (Atom "P" []) (Atom "Q" []))
  print (comm (Atom "P" []) (Atom "R" []))
  print (comm (Atom "Q" []) (Atom "R" []))

data Verdict = Provable | ProvableNonGround | NotApplicable | NoSolutions deriving (Eq, Show)

isGroundTerm :: Term -> Bool
isGroundTerm t = case t of { TVar _ -> False; TFun _ xs -> all isGroundTerm xs }

isGroundSubstFor :: [Name] -> Subst -> Bool
isGroundSubstFor xs s = all (\x -> isGroundTerm (applyTerm s (TVar x))) xs

runSolveCore :: Bool -> Comm -> Int -> QProgram -> Goal -> [Subst]
runSolveCore dbg comm maxSol qprog goal = take maxSol (solveQLPWithDebug dbg comm qprog goal emptySubst 0)

classifyHilbert :: Bool -> [Name] -> Comm -> Comm -> Int -> QProgram -> Goal -> (Verdict, [Subst], [Subst])
classifyHilbert dbg xs commHilbert commAlways maxSol qprog goal =
  let solsH = runSolveCore dbg commHilbert maxSol qprog goal
      solsA = runSolveCore dbg commAlways  maxSol qprog goal
  in if not (null solsH) then (if any (not . isGroundSubstFor xs) solsH then ProvableNonGround else Provable, solsH, solsA) else if not (null solsA) then (NotApplicable, [], solsA) else (NoSolutions, [], [])

pairs2 :: [a] -> [(a,a)]
pairs2 [] = []
pairs2 (x:xs) = [(x,y) | y <- xs] ++ pairs2 xs

firstCommFailInAtoms :: Comm -> [Atom] -> Maybe (Atom, Atom)
firstCommFailInAtoms comm atoms =
  let bad (a,b) = not (comm a b)
  in case filter bad (pairs2 atoms) of { [] -> Nothing; (p:_) -> Just p }

-- NotApplicable の原因は、goal 内の原子ではなく、goal を導く clause の body（negAtoms）内の非可換ペアであることが多い。
-- そこで、always 解（s0）で goal を具体化し、goal と unify できる head を持つ clause を探し、その body 内の最初の非可換ペアを返す。
firstCommFailFromQProg :: Comm -> QProgram -> Goal -> Subst -> Maybe (Atom, Atom)
firstCommFailFromQProg comm qprog goal s0 =
  let goalAtoms = map (applyAtom s0) (wantPos goal ++ wantNeg goal)
      inst su a = applyAtom s0 (applyAtom su a)
      tryClause (Clause neg pos) =
        let tryHead h =
              let tryGoal g =
                    case unifyAtom h g emptySubst of
                      Nothing -> Nothing
                      Just su ->
                        case firstCommFailInAtoms comm (map (inst su) neg) of
                          Just p  -> Just p
                          Nothing -> firstCommFailInAtoms comm (map (inst su) (neg ++ pos))
              in foldr (\g acc -> case acc of { Just _ -> acc; Nothing -> tryGoal g }) Nothing goalAtoms
        in foldr (\h acc -> case acc of { Just _ -> acc; Nothing -> tryHead h }) Nothing pos
      hit = foldr (\cl acc -> case acc of { Just _ -> acc; Nothing -> tryClause cl }) Nothing qprog
      fallback1 = foldr (\(Clause neg _pos) acc -> case acc of { Just _ -> acc; Nothing -> firstCommFailInAtoms comm (map (applyAtom s0) neg) }) Nothing qprog
  in case hit of { Just p -> Just p; Nothing -> fallback1 }

classifyHilbertWithReason :: Bool -> [Name] -> Comm -> Comm -> Int -> QProgram -> Goal -> (Verdict, [Subst], [Subst], Maybe (Atom, Atom))
classifyHilbertWithReason dbg xs commHilbert commAlways maxSol qprog goal =
  let solsH = runSolveCore dbg commHilbert maxSol qprog goal
      solsA = runSolveCore dbg commAlways  maxSol qprog goal
  in if not (null solsH) then (if any (not . isGroundSubstFor xs) solsH then ProvableNonGround else Provable, solsH, solsA, Nothing) else case solsA of { (s0:_) -> (NotApplicable, [], solsA, firstCommFailFromQProg commHilbert qprog goal s0); [] -> (NoSolutions, [], [], Nothing) }

selectComm :: Bool -> FilePath -> String -> Maybe FilePath -> IO (Either String (Comm, Comm))
selectComm _dbg modelPath commMode0 commFactsPath = do
  let commMode = map toLower commMode0
  let commAlways :: Comm
      commAlways = \_ _ -> True
  case commMode of
    "always" -> pure (Right (commAlways, commAlways))
    "facts" ->
      case commFactsPath of
        Nothing -> pure (Left "Missing --comm-facts <path> for --comm facts.")
        Just fp -> do
          ef <- readCommFacts fp
          case ef of
            Left msg -> pure (Left msg)
            Right facts -> pure (Right (mkCommFacts facts, commAlways))
    "pauli" -> do
      ep <- mkCommPauliFromModelFile modelPath
      case ep of
        Left msg -> pure (Left ("Failed to load pauli model: " ++ msg))
        Right commPauli -> pure (Right (commPauli, commAlways))
    _ -> do
      model <- loadModelOrDefault modelPath
      pure (Right (commutes model, commAlways))

runSolve :: Bool -> FilePath -> String -> Maybe FilePath -> Int -> FilePath -> FilePath -> IO ()
runSolve dbg modelPath commMode0 commFactsPath maxSol qprogPath goalPath = do
  e <- readQProgGoal qprogPath goalPath
  case e of
    Left msg -> putStrLn msg
    Right (qprog, goal) -> do
      let xs = goalVars goal
      ec <- selectComm dbg modelPath commMode0 commFactsPath
      case ec of
        Left msg -> putStrLn msg
        Right (commH, commAlways) -> do
          let commMode = map toLower commMode0
          case commMode of
            "always" -> do
              let sols = runSolveCore dbg commAlways maxSol qprog goal
              if null sols then putStrLn "(no solutions)" else mapM_ (putStrLn . showSubstFor xs) sols
            "facts" -> do
              let sols = runSolveCore dbg commH maxSol qprog goal
              if null sols then putStrLn "(no solutions)" else mapM_ (putStrLn . showSubstFor xs) sols
            _ -> do
              let (v, solsH, solsA) = classifyHilbert dbg xs commH commAlways maxSol qprog goal
              case v of
                Provable -> do { putStrLn "(provable)"; mapM_ (putStrLn . showSubstFor xs) solsH }
                ProvableNonGround -> do { putStrLn "(provable-nonground)"; mapM_ (putStrLn . showSubstFor xs) solsH }
                NotApplicable -> do { putStrLn "(not-applicable)"; if null solsA then pure () else mapM_ (putStrLn . showSubstFor xs) solsA }
                NoSolutions -> putStrLn "(no solutions)"

runCompare :: Bool -> FilePath -> String -> Maybe FilePath -> Int -> FilePath -> FilePath -> IO ()
runCompare dbg modelPath commMode0 commFactsPath maxSol qprogPath goalPath = do
  e <- readQProgGoal qprogPath goalPath
  case e of
    Left msg -> putStrLn msg
    Right (qprog, goal) -> do
      let xs = goalVars goal
      ec <- selectComm dbg modelPath commMode0 commFactsPath
      case ec of
        Left msg -> putStrLn msg
        Right (commH, commAlways) -> do
          let (v, solsH, solsA) = classifyHilbert dbg xs commH commAlways maxSol qprog goal
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

atomPred :: Atom -> String
atomPred (Atom p _) = p

jmaybePair :: Maybe (Atom, Atom) -> String
jmaybePair mp = case mp of { Nothing -> "null"; Just (a,b) -> "[" ++ jstr (atomPred a) ++ "," ++ jstr (atomPred b) ++ "]" }

verdictText :: Verdict -> String
verdictText v = case v of { Provable -> "provable"; ProvableNonGround -> "provable-nonground"; NotApplicable -> "not-applicable"; NoSolutions -> "no-solutions" }

compareJsonLine :: FilePath -> FilePath -> FilePath -> Int -> [Name] -> Verdict -> [Subst] -> [Subst] -> Maybe (Atom, Atom) -> String
compareJsonLine modelPath qprogPath goalPath maxSol xs v solsH solsA failPair =
  let sh = map (showSubstFor xs) solsH
      sa = map (showSubstFor xs) solsA
  in "{" ++ intercalate "," [ "\"model\":" ++ jstr modelPath, "\"qprog\":" ++ jstr qprogPath, "\"goal\":" ++ jstr goalPath, "\"max_sol\":" ++ show maxSol, "\"vars\":" ++ jarrStr xs, "\"verdict\":" ++ jstr (verdictText v), "\"sols_hilbert\":" ++ jarrStr sh, "\"sols_always\":" ++ jarrStr sa, "\"comm_fail_pair\":" ++ jmaybePair failPair ] ++ "}"

runCompareJsonl :: Bool -> FilePath -> String -> Maybe FilePath -> Int -> FilePath -> FilePath -> FilePath -> IO ()
runCompareJsonl dbg modelPath commMode0 commFactsPath maxSol outPath qprogPath goalPath = do
  e <- readQProgGoal qprogPath goalPath
  case e of
    Left msg -> putStrLn msg
    Right (qprog, goal) -> do
      let xs = goalVars goal
      ec <- selectComm dbg modelPath commMode0 commFactsPath
      case ec of
        Left msg -> putStrLn msg
        Right (commH, commAlways) -> do
          let (v, solsH, solsA, failPair) = classifyHilbertWithReason dbg xs commH commAlways maxSol qprog goal
          let line = compareJsonLine modelPath qprogPath goalPath maxSol xs v solsH solsA failPair
          appendFile outPath (line ++ "\n")
          case v of
            Provable -> do { putStrLn "(provable)"; mapM_ (putStrLn . showSubstFor xs) solsH }
            ProvableNonGround -> do { putStrLn "(provable-nonground)"; mapM_ (putStrLn . showSubstFor xs) solsH }
            NotApplicable -> do { putStrLn "(not-applicable)"; if null solsA then pure () else mapM_ (putStrLn . showSubstFor xs) solsA }
            NoSolutions -> putStrLn "(no solutions)"

commCheckJsonLine :: FilePath -> String -> String -> String -> Bool -> String
commCheckJsonLine modelPath commMode a b ok =
  "{" ++ intercalate "," [ "\"model\":" ++ jstr modelPath, "\"comm_mode\":" ++ jstr commMode, "\"a\":" ++ jstr a, "\"b\":" ++ jstr b, "\"comm\":" ++ (if ok then "true" else "false") ] ++ "}"

-- Parse Atom arguments for `comm-check`.
-- Accepted forms:
--   * Paper-like / comm-facts syntax: Q, Q(a), Q(X), Q(f(a,b)), ...
--   * Haskell Show/Read form: Atom "Q" [TFun "a" []], ...
parseAtomArg :: String -> Either String Atom
parseAtomArg s =
  case (readMaybe s :: Maybe Atom) of
    Just a  -> Right a
    Nothing ->
      case parseCommFactsText ("comm(" ++ s ++ ",DUMMY).") of
        Right ((a,_):_) -> Right a
        Left e          -> Left (e ++ " (input: " ++ show s ++ ")")


runCommCheck :: Bool -> FilePath -> String -> Maybe FilePath -> String -> String -> IO ()
runCommCheck dbg modelPath commMode0 commFactsPath a b = do
  ec <- selectComm dbg modelPath commMode0 commFactsPath
  case ec of
    Left msg -> putStrLn msg
    Right (commH, _commAlways) ->
      case (parseAtomArg a, parseAtomArg b) of
        (Left e, _) -> putStrLn ("Failed to parse Atom1 for comm-check: " ++ e)
        (_, Left e) -> putStrLn ("Failed to parse Atom2 for comm-check: " ++ e)
        (Right a1, Right b1) -> do
          let ok = commH a1 b1
          putStrLn (commCheckJsonLine modelPath (map toLower commMode0) a b ok)


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

extractDebugArg :: [String] -> (Bool, [String])
extractDebugArg [] = (False, [])
extractDebugArg ("--debug":xs) = let (b, ys) = extractDebugArg xs in (True || b, ys)
extractDebugArg ("--trace":xs) = let (b, ys) = extractDebugArg xs in (True || b, ys)
extractDebugArg (x:xs) = let (b, ys) = extractDebugArg xs in (b, x:ys)

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

data Cmd = CmdRun | CmdEmitSample FilePath FilePath | CmdSolve FilePath FilePath | CmdCompare FilePath FilePath | CmdCompareJsonl FilePath FilePath FilePath | CmdCommCheck String String | CmdHelp deriving (Eq, Show)

parseCmd :: [String] -> Either String Cmd
parseCmd rest =
  case rest of
    [] -> Right CmdHelp
    ["--help"] -> Right CmdHelp
    ["-h"] -> Right CmdHelp
    ["help"] -> Right CmdHelp
    ["--run"] -> Right CmdRun
    ["run"] -> Right CmdRun
    ("--emit-sample":qprogPath:goalPath:[]) -> Right (CmdEmitSample qprogPath goalPath)
    ("emit-sample":qprogPath:goalPath:[]) -> Right (CmdEmitSample qprogPath goalPath)
    ("--solve":qprogPath:goalPath:[]) -> Right (CmdSolve qprogPath goalPath)
    ("solve":qprogPath:goalPath:[]) -> Right (CmdSolve qprogPath goalPath)
    ("--compare":qprogPath:goalPath:[]) -> Right (CmdCompare qprogPath goalPath)
    ("compare":qprogPath:goalPath:[]) -> Right (CmdCompare qprogPath goalPath)
    ("--compare-jsonl":outPath:qprogPath:goalPath:[]) -> Right (CmdCompareJsonl outPath qprogPath goalPath)
    ("compare-jsonl":outPath:qprogPath:goalPath:[]) -> Right (CmdCompareJsonl outPath qprogPath goalPath)
    ("--comm-check":a:b:[]) -> Right (CmdCommCheck a b)
    ("comm-check":a:b:[]) -> Right (CmdCommCheck a b)
    _ -> Left ("Unknown/invalid arguments: " ++ show rest)

printUsage :: IO ()
printUsage = do
  putStrLn "Usage:"
  putStrLn "  qlp [options] solve <qprogPath> <goalPath>"
  putStrLn "  qlp [options] compare <qprogPath> <goalPath>"
  putStrLn "  qlp [options] compare-jsonl <outPath> <qprogPath> <goalPath>"
  putStrLn "  qlp [options] emit-sample <qprogPath> <goalPath>"
  putStrLn "  qlp [options] run"
  putStrLn "  qlp [options] comm-check <Atom1> <Atom2>"
  putStrLn "  qlp help"
  putStrLn "Legacy forms (still accepted):"
  putStrLn "  qlp [options] --solve <qprogPath> <goalPath>"
  putStrLn "  qlp [options] --compare <qprogPath> <goalPath>"
  putStrLn "  qlp [options] --compare-jsonl <outPath> <qprogPath> <goalPath>"
  putStrLn "  qlp [options] --emit-sample <qprogPath> <goalPath>"
  putStrLn "  qlp [options] --run"
  putStrLn "Options:"
  putStrLn "  --model <path>                      (default: hilbert.conf)"
  putStrLn "  --comm <hilbert|pauli|always|facts> (default: hilbert)"
  putStrLn "  --comm-facts <path>                 (required when --comm facts)"
  putStrLn "  --max-sol <n>                       (default: 20)"
  putStrLn "  --debug                             (print comm-fail traces; uses Debug.Trace)"
  putStrLn "Notes:"
  putStrLn "  The first token may be a subcommand (solve/compare/...) or legacy flags (--solve/...)."
  putStrLn "  If argument parsing fails, this program prints args and exits with failure."

main :: IO ()
main = do
  args <- getArgs
  let (dbg, args0)       = extractDebugArg args
  let (modelPath, args1) = extractModelArg args0
  let (commMode, args2)  = extractCommArg args1
  let (commFacts, args3) = extractCommFactsArg args2
  let (maxSol, rest)     = extractMaxSolArg args3
  cmdE <- pure (parseCmd rest)
  case cmdE of
    Left err -> do { putStrLn err; putStrLn ("args = " ++ show args); printUsage; exitFailure }
    Right CmdHelp -> do { printUsage; putStrLn ("args = " ++ show args) }
    Right CmdRun -> runSmoke dbg modelPath commMode
    Right (CmdEmitSample qprogPath goalPath) -> runEmitSample qprogPath goalPath
    Right (CmdSolve qprogPath goalPath) -> runSolve dbg modelPath commMode commFacts maxSol qprogPath goalPath
    Right (CmdCompare qprogPath goalPath) -> runCompare dbg modelPath commMode commFacts maxSol qprogPath goalPath
    Right (CmdCompareJsonl outPath qprogPath goalPath) -> runCompareJsonl dbg modelPath commMode commFacts maxSol outPath qprogPath goalPath
    Right (CmdCommCheck a b) -> runCommCheck dbg modelPath commMode commFacts a b
