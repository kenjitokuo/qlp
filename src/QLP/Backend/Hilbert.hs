module QLP.Backend.Hilbert
  ( HilbertModel
  , Comm(..)
  , loadModelOrDefault
  , commutes
  , commutes3
  , modelKeys
  ) where

import qualified Data.Map.Strict as M
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)
import Control.Exception (try, IOException)
import Text.Read (readMaybe)
import QLP.Syntax (Atom(..), Term(..))

type Key   = String
type Vec   = [Double]
type Space = [Vec]

data HilbertModel = HilbertModel
  { hmDim   :: Int
  , hmEps   :: Double
  , hmSpace :: M.Map Key Space
  } deriving (Eq, Show)

data Comm = CommTrue | CommFalse | CommUnknown
  deriving (Eq, Show)

modelKeys :: HilbertModel -> [Key]
modelKeys = M.keys . hmSpace

commutes :: HilbertModel -> Atom -> Atom -> Bool
commutes m a b =
  case commutes3 m a b of
    CommFalse -> False
    _         -> True

commutes3 :: HilbertModel -> Atom -> Atom -> Comm
commutes3 m a b =
  case (lookupSpace m a, lookupSpace m b) of
    (Just sa, Just sb) ->
      if commutesSpace (hmEps m) (hmDim m) sa sb then CommTrue else CommFalse
    _ -> CommUnknown

lookupSpace :: HilbertModel -> Atom -> Maybe Space
lookupSpace m a@(Atom p _) =
  let mp    = hmSpace m
      kPred = normKey p
      kWild = wildKey a
  in case atomKey a of
       Just kExact -> M.lookup kExact mp <|> M.lookup kWild mp <|> M.lookup kPred mp
       Nothing     -> M.lookup kWild  mp <|> M.lookup kPred mp

wildKey :: Atom -> Key
wildKey (Atom p ts) =
  let p' = normKey p
      n  = length ts
  in if n == 0 then p' else p' ++ "(" ++ intercalate "," (replicate n "_") ++ ")"

atomKey :: Atom -> Maybe Key
atomKey (Atom p ts) =
  if all isGroundTerm ts
    then
      let p' = normKey p
      in Just (if null ts then p' else p' ++ "(" ++ intercalate "," (map termKey ts) ++ ")")
    else Nothing

isGroundTerm :: Term -> Bool
isGroundTerm t =
  case t of
    TVar _    -> False
    TFun _ as -> all isGroundTerm as

termKey :: Term -> String
termKey t =
  case t of
    TVar x    -> x
    TFun f [] -> normKey f
    TFun f as -> normKey f ++ "(" ++ intercalate "," (map termKey as) ++ ")"


-- --------------------
-- Commutativity checks (inner products only)
-- --------------------

commutesSpace :: Double -> Int -> Space -> Space -> Bool
commutesSpace eps d sa0 sb0 =
  let sa = gramSchmidt (max 1e-12 (eps * 0.1)) d sa0
      sb = gramSchmidt (max 1e-12 (eps * 0.1)) d sb0
  in case (sa, sb) of
       ([va], [vb]) -> commutesKet eps d va vb
       _            -> commutesSubspace eps d sa sb

-- Rank-1: P_v and P_w commute iff v ? w or v ∥ w
commutesKet :: Double -> Int -> Vec -> Vec -> Bool
commutesKet eps d vp0 vq0 =
  let vp = normalizeVec d vp0
      vq = normalizeVec d vq0
      c  = abs (dot vp vq)
  in c <= eps || abs (c - 1) <= eps

-- General: test mutual invariance using projections onto orthonormal bases
commutesSubspace :: Double -> Int -> Space -> Space -> Bool
commutesSubspace eps d u v =
  let okU = all (\x -> inSpan eps d u (projOn d v x)) u
      okV = all (\x -> inSpan eps d v (projOn d u x)) v
  in okU && okV

projOn :: Int -> Space -> Vec -> Vec
projOn d bs x =
  foldl vecAdd (zeroVec d) [ vecScale (dot x b) b | b <- bs ]

inSpan :: Double -> Int -> Space -> Vec -> Bool
inSpan eps d bs x =
  let px = projOn d bs x
  in norm (vecSub x px) <= eps

gramSchmidt :: Double -> Int -> Space -> Space
gramSchmidt eps d vs0 = go [] (map (pad d) vs0)
  where
    go acc [] = reverse acc
    go acc (v:vs) =
      let v1 = foldl vecSub v [ vecScale (dot v u) u | u <- acc ]
          n2 = dot v1 v1
      in if n2 <= eps * eps
           then go acc vs
           else go (vecScale (1 / sqrt n2) v1 : acc) vs

-- --------------------
-- Vector ops
-- --------------------

pad :: Int -> Vec -> Vec
pad d v = take d (v ++ repeat 0)

zeroVec :: Int -> Vec
zeroVec d = replicate d 0

vecAdd :: Vec -> Vec -> Vec
vecAdd = zipWith (+)

vecSub :: Vec -> Vec -> Vec
vecSub = zipWith (-)

vecScale :: Double -> Vec -> Vec
vecScale a = map (a *)

dot :: Vec -> Vec -> Double
dot xs ys = sum (zipWith (*) xs ys)

norm :: Vec -> Double
norm v = sqrt (dot v v)

normalizeVec :: Int -> Vec -> Vec
normalizeVec d v0 =
  let v  = pad d v0
      n2 = dot v v
  in if n2 <= 0 then v else vecScale (1 / sqrt n2) v

-- --------------------
-- Loading/parsing
-- --------------------

loadModelOrDefault :: FilePath -> IO HilbertModel
loadModelOrDefault fp = do
  e <- try (readFile fp) :: IO (Either IOException String)
  case e of
    Left _    -> pure defaultModel
    Right txt ->
      case parseModel txt of
        Just m  -> pure m
        Nothing -> pure defaultModel

defaultModel :: HilbertModel
defaultModel =
  let d   = 2
      eps = 1e-9
      mp  = M.fromList
              [ ("p", [ket0])
              , ("q", [ket0])
              , ("r", [ketPlus])
              ]
  in HilbertModel d eps (M.map (map (normalizeVec d)) mp)

parseModel :: String -> Maybe HilbertModel
parseModel txt =
  let ls0 = map strip (lines txt)
      ls1 = filter (not . null) (map stripComment ls0)
      kvs = mapMaybe parseKV ls1
      dim = lookupInt "dim" kvs <|> lookupInt "dimension" kvs
      eps = lookupDouble "eps" kvs <|> lookupDouble "epsilon" kvs
      d   = maybe 2 id dim
      e   = maybe 1e-9 id eps
      sp0 = M.fromList (mapMaybe (parseSpaceLine d) kvs)
      sp  = M.map (map (normalizeVec d)) sp0
  in Just (HilbertModel d e sp)

parseSpaceLine :: Int -> (String, String) -> Maybe (Key, Space)
parseSpaceLine d (k,v)
  | k == "dim" || k == "dimension" || k == "eps" || k == "epsilon" = Nothing
  | otherwise = do
      sp <- parseSpace d v
      Just (k, sp)

parseSpace :: Int -> String -> Maybe Space
parseSpace d s =
  case (readMaybe s :: Maybe [[Double]]) of
    Just vss -> Just (map (normalizeVec d) vss)
    Nothing  ->
      case (readMaybe s :: Maybe [Double]) of
        Just vs -> Just [normalizeVec d vs]
        Nothing ->
          case parseKet d s of
            Just vs -> Just [normalizeVec d vs]
            Nothing -> Nothing

parseKet :: Int -> String -> Maybe Vec
parseKet d s0 =
  let s = normKey s0
  in case (d, s) of
       (2, "ket0")     -> Just ket0
       (2, "ket1")     -> Just ket1
       (2, "ketplus")  -> Just ketPlus
       (2, "ketminus") -> Just ketMinus
       (2, "0")        -> Just ket0
       (2, "1")        -> Just ket1
       (2, "+")        -> Just ketPlus
       (2, "-")        -> Just ketMinus
       _               -> Nothing

ket0, ket1, ketPlus, ketMinus :: Vec
ket0     = [1, 0]
ket1     = [0, 1]
ketPlus  = let a = 1 / sqrt 2 in [a, a]
ketMinus = let a = 1 / sqrt 2 in [a, -a]

-- --------------------
-- Small helpers
-- --------------------

stripComment :: String -> String
stripComment s = takeWhile (/= '#') s

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

normKey :: String -> String
normKey = map toLowerASCII . filter (not . isSpace) . strip

toLowerASCII :: Char -> Char
toLowerASCII c
  | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise            = c

splitOnce :: Char -> String -> Maybe (String, String)
splitOnce ch s =
  case break (== ch) s of
    (a, _ : b) -> Just (a, b)
    _          -> Nothing

parseKV :: String -> Maybe (String, String)
parseKV s =
  case splitOnce ':' s of
    Just (k,v) -> Just (normKey k, strip v)
    Nothing ->
      case splitOnce '=' s of
        Just (k,v) -> Just (normKey k, strip v)
        Nothing    -> Nothing

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> case f x of { Just y -> y : acc; Nothing -> acc }) []

lookupInt :: String -> [(String,String)] -> Maybe Int
lookupInt k kvs = do
  v <- lookup k kvs
  readMaybe v

lookupDouble :: String -> [(String,String)] -> Maybe Double
lookupDouble k kvs = do
  v <- lookup k kvs
  readMaybe v

infixl 3 <|>
(<|>) :: Maybe a -> Maybe a -> Maybe a
(<|>) a b = case a of { Just _ -> a; Nothing -> b }
