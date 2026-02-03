module QLP.Backend.Hilbert where

import Data.Complex (Complex((:+)))
import qualified Data.Map.Strict as M
import QLP.Syntax
import System.IO (withFile, IOMode(ReadMode), hSetEncoding, utf8, hGetContents)
import Control.Exception (evaluate)
import System.Directory (doesFileExist)


data HilbertModel = HilbertModel
  { dim :: Int
  , predProj :: M.Map String Mat
  , epsComm :: Double
  } deriving (Eq, Show)

type C = Complex Double
type Mat = [[C]]  -- very small, assume square dim x dim

defaultModel :: HilbertModel
defaultModel =
  HilbertModel
    { dim = 2
    , predProj = M.fromList
        [ ("P", projZ0)
        , ("Q", projZ1)
        , ("R", projXp)
        , ("S", projXm)
        ]
    , epsComm = 1e-9
    }

-- Basic matrices (2x2)
iC :: C
iC = 0 :+ 1

projZ0 :: Mat
projZ0 = [[1:+0, 0:+0],
          [0:+0, 0:+0]]

projZ1 :: Mat
projZ1 = [[0:+0, 0:+0],
          [0:+0, 1:+0]]

-- |+> = (1/sqrt2)(|0>+|1|),  |-> = (1/sqrt2)(|0>-|1>)
-- Projectors: |+><+| and |-><-|
projXp :: Mat
projXp = [[0.5:+0, 0.5:+0],
          [0.5:+0, 0.5:+0]]

projXm :: Mat
projXm = [[0.5:+0, (-0.5):+0],
          [(-0.5):+0, 0.5:+0]]
preset :: String -> Maybe Mat
preset name = case name of
  "Z0" -> Just projZ0
  "Z1" -> Just projZ1
  "X+" -> Just projXp
  "X-" -> Just projXm
  _    -> Nothing

zeroMat :: Int -> Mat
zeroMat n = replicate n (replicate n (0:+0))

matSub :: Mat -> Mat -> Mat
matSub a b = zipWith (zipWith (-)) a b

matMul :: Mat -> Mat -> Mat
matMul a b =
  let bt = transpose b
  in [[ sum [ aik * bkj | (aik, bkj) <- zip ar bc ] | bc <- bt ] | ar <- a]

transpose :: Mat -> Mat
transpose xs =
  case sequence (map uncons xs) of
    Nothing -> []
    Just ys ->
      let hs = map fst ys
          ts = map snd ys
      in hs : transpose ts

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)


frobenius :: Mat -> Double
frobenius a =
  sqrt (sum [ magnitude z * magnitude z | row <- a, z <- row ])

magnitude :: C -> Double
magnitude (x :+ y) = sqrt (x*x + y*y)

-- Map an atom to a projector by its predicate symbol (placeholder).
-- Later: use also arguments/parameters if needed.
atomProj :: HilbertModel -> Atom -> Mat
atomProj model (Atom p _) =
  M.findWithDefault projZ0 p (predProj model)

-- Check commutativity via commutator norm.
commutes :: HilbertModel -> Atom -> Atom -> Bool
commutes model a b =
  let pa = atomProj model a
      pb = atomProj model b
      comm = matSub (matMul pa pb) (matMul pb pa)
  in frobenius comm < epsComm model

-- Very small config parser (no extra deps).
-- Lines:
--   dim N
--   eps E
--   pred P Z0|Z1|X+|X-

loadModelFromFile :: FilePath -> IO HilbertModel
loadModelFromFile fp = do
  txt0 <- withFile fp ReadMode $ \h -> do
    hSetEncoding h utf8
    s <- hGetContents h
    _ <- evaluate (length s)   -- ‚±‚±‚Å‘S•¶‚ð“Ç‚ÝØ‚é
    pure s
  let txt = dropBOM txt0
      ls = filter (not . null) (map strip (lines txt))
  pure (applyLines defaultModel ls)


dropBOM :: String -> String
dropBOM ('\xFEFF':xs) = xs
dropBOM xs = xs


applyLines :: HilbertModel -> [String] -> HilbertModel
applyLines m [] = m
applyLines m (l:ls) =
  case words l of
    ("dim":[nStr]) ->
      applyLines (m { dim = read nStr }) ls
    ("eps":[eStr]) ->
      applyLines (m { epsComm = read eStr }) ls
    ("pred":[p, tag]) ->
      case preset tag of
        Nothing -> applyLines m ls
        Just mat -> applyLines (m { predProj = M.insert p mat (predProj m) }) ls
    _ -> applyLines m ls

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\r'

loadModelOrDefault :: FilePath -> IO HilbertModel
loadModelOrDefault fp = do
  ok <- doesFileExist fp
  if ok then loadModelFromFile fp else pure defaultModel
