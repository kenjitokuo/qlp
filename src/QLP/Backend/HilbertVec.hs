module QLP.Backend.HilbertVec
  ( VecModel(..)
  , defaultVecModel
  , commutesVec
  , loadVecModelOrDefault
  , writeVecModelSample
  ) where

import qualified Data.Map.Strict as M
import Data.Char (isSpace)
import Text.Read (readMaybe)
import System.IO.Error (catchIOError)
import QLP.Syntax (Atom(..))

data VecModel = VecModel
  { vmDim  :: Int
  , vmEps  :: Double
  , vmVecs :: M.Map String [Double]
  } deriving (Eq, Show, Read)

defaultVecModel :: VecModel
defaultVecModel =
  let dim = 2
      eps = 1e-6
      raw = M.fromList
        [ ("P", [1,0])
        , ("Q", [1,0])
        , ("R", [1,1])
        ]
  in VecModel dim eps (M.map (normalize dim) raw)

atomKey :: Atom -> String
atomKey (Atom p _) = p

commutesVec :: VecModel -> Atom -> Atom -> Bool
commutesVec vm a b =
  let ka = atomKey a
      kb = atomKey b
  in case (M.lookup ka (vmVecs vm), M.lookup kb (vmVecs vm)) of
       (Just u, Just v) ->
         let d = abs (dot u v)
             eps = vmEps vm
         in d <= eps || d >= 1 - eps
       _ -> True

dot :: [Double] -> [Double] -> Double
dot xs ys = sum (zipWith (*) xs ys)

norm :: [Double] -> Double
norm xs = sqrt (dot xs xs)

normalize :: Int -> [Double] -> [Double]
normalize dim xs =
  let ys = take dim xs
      n  = norm ys
  in if n == 0 then ys else map (/ n) ys

stripComment :: String -> String
stripComment [] = []
stripComment ('#':_) = []
stripComment (c:cs) = c : stripComment cs

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

parseVecModel :: String -> Either String VecModel
parseVecModel txt =
  go (VecModel 0 1e-6 M.empty) (lines txt)
  where
    go vm [] =
      let dim0 = if vmDim vm <= 0 then inferDim (vmVecs vm) else vmDim vm
          vm1  = vm { vmDim = dim0, vmVecs = M.map (normalize dim0) (vmVecs vm) }
      in if dim0 <= 0 then Left "VecModel: missing dimension (add a line like: dim 2)."
         else Right vm1

    go vm (ln:rest) =
      let line = trim (stripComment ln)
      in if null line then go vm rest else
         case words line of
           [] -> go vm rest

           ("dim":n:[]) ->
             case readMaybe n of
               Just d | d > 0 -> go (vm { vmDim = d }) rest
               _ -> Left ("VecModel: invalid dim line: " ++ ln)

           ("eps":e:[]) ->
             case readMaybe e of
               Just x | x > 0 -> go (vm { vmEps = x }) rest
               _ -> Left ("VecModel: invalid eps line: " ++ ln)

           (name:nums) ->
             case traverse readMaybe nums of
               Nothing -> Left ("VecModel: invalid vector line: " ++ ln)
               Just xs ->
                 let dim0 = if vmDim vm <= 0 then length xs else vmDim vm
                     xs1  = take dim0 xs
                 in if length xs1 /= dim0 then Left ("VecModel: wrong dimension in line: " ++ ln)
                    else go (vm { vmDim = dim0, vmVecs = M.insert name xs1 (vmVecs vm) }) rest

inferDim :: M.Map String [Double] -> Int
inferDim mp =
  case M.elems mp of
    (v:_) -> length v
    []    -> 0

loadVecModelOrDefault :: FilePath -> IO VecModel
loadVecModelOrDefault path =
  catchIOError
    (do txt <- readFile path
        case parseVecModel txt of
          Right vm -> pure vm
          Left _   -> pure defaultVecModel)
    (\_ -> pure defaultVecModel)

writeVecModelSample :: FilePath -> IO ()
writeVecModelSample path = do
  let sample = unlines
        [ "# Vector Hilbert model sample"
        , "dim 2"
        , "eps 1e-6"
        , "P 1 0"
        , "Q 1 0"
        , "R 1 1"
        ]
  writeFile path sample
