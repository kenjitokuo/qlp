module QLP.Backend.Pauli
  ( PauliModel(..)
  , isPauliChar
  , commutesPauli
  , parsePauliModelText
  , lookupPauli
  , commPauli
  ) where

-- Pauli backend: comm(u,v) is decided by the Pauli strings P(u), P(v) given in a model file.
-- Two Pauli operators commute iff the number of positions where both symbols differ from I
-- and are different from each other is even (global phases are ignored).

import qualified Data.Map.Strict as M
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd)
import Text.Read (readMaybe)
import QLP.Syntax (Atom(..))
import QLP.Search (Comm)
import QLP.Parser (parseAtomText)

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

stripComment :: String -> String
stripComment = takeWhile (/= '#')

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

-- Pauli model file: "n = <int>" and entries "<atom> = <word>", where <atom> is written in the
-- text syntax (e.g. "XII", "u(a)", "Q(a)") and <word> is a Pauli string over {I,X,Y,Z}.
-- Other key=value lines (dim=..., eps=..., q = ket0, ...) are ignored so that mixed conf files
-- do not break.  An entry whose left-hand side has arguments is always treated as a Pauli entry,
-- so a malformed word there is reported as an error instead of being silently ignored.
data PauliModel = PauliModel
  { pmExact :: M.Map Atom String   -- entries keyed by the atom as written
  , pmPred  :: M.Map String String -- predicate-level fallback (later entries win)
  }

parsePauliModelText :: String -> Either String (Int, PauliModel)
parsePauliModelText s =
  let ls  = [trim (stripComment l) | l <- lines s]
      ls1 = filter (not . null) ls
      isPauliWord w = not (null w) && all isPauliChar w
      step (mn, es) l =
        case break (=='=') l of
          (lhs,'=':rhs0) ->
            let lhs1 = trim lhs
                w    = normalizePauliWord rhs0
                lhsLower = map toLower lhs1
            in if lhsLower == "n" then
                 case readMaybe (trim rhs0) of
                   Nothing -> Left ("Failed to parse n in model: " ++ l)
                   Just k  -> Right (Just k, es)
               else case parseAtomText lhs1 of
                 Left _ -> Right (mn, es)
                 Right at
                   | isPauliWord w || '(' `elem` lhs1 -> Right (mn, (at, w) : es)
                   | otherwise -> Right (mn, es)
          _ -> Right (mn, es)  -- '=' がない行は無視
      go (mn, es) [] = case mn of { Nothing -> Left "Missing n = <int> in model"; Just k -> Right (k, reverse es) }
      go st (l:rest) = do { st' <- step st l; go st' rest }
  in do
    (n, es) <- go (Nothing, []) ls1
    let badLen  = [(show k,w) | (k,w) <- es, length w /= n]
    let badChar = [(show k,w) | (k,w) <- es, any (not . isPauliChar) w]
    if not (null badChar) then Left ("Pauli word has invalid chars (allowed {I,X,Y,Z}): " ++ show badChar)
    else if not (null badLen) then Left ("Pauli word length mismatch (expected n=" ++ show n ++ "): " ++ show badLen)
    else Right (n, PauliModel (M.fromList es) (M.fromList [ (p, w) | (Atom p _, w) <- es ]))

-- Look up the Pauli string of an atom: the exact (instantiated) atom first, then the predicate.
lookupPauli :: PauliModel -> Atom -> Maybe String
lookupPauli pm a@(Atom p _) =
  case M.lookup a (pmExact pm) of
    Just w  -> Just w
    Nothing -> M.lookup p (pmPred pm)


-- comm(u,v) for the Pauli backend.  Atoms not listed in the model are treated as commuting
-- (same policy as CommUnknown in the Hilbert backend).
commPauli :: PauliModel -> Comm
commPauli pm a b =
  case (lookupPauli pm a, lookupPauli pm b) of
    (Just w1, Just w2) ->
      case commutesPauli w1 w2 of
        Right r  -> r
        Left msg -> error ("commutesPauli: " ++ msg)
    _ -> True
