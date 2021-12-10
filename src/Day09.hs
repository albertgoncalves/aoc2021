import Data.Char (isDigit, ord)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch1,
    readP_to_S,
  )
import Prelude hiding (lookup)

data Coord = Coord Int Int
  deriving (Eq, Ord, Show)

newline :: ReadP Char
newline = char '\n'

digits :: ReadP [Int]
digits = map (subtract (ord '0') . ord) <$> munch1 isDigit <* newline

parse :: String -> [[Int]]
parse = fst . head . readP_to_S (many1 digits <* eof)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

intoMap :: [[Int]] -> M.Map Coord Int
intoMap rows =
  M.fromList
    [(Coord j i, x) | (i, row) <- enumerate rows, (j, x) <- enumerate row]

check ::
  M.Map Coord Int ->
  Coord ->
  (S.Set Coord, [Coord]) ->
  (S.Set Coord, [Coord])
check m x@(Coord j i) y@(s, xs)
  | S.member x s = y
  | all ((m M.! x) <) $ mapMaybe (`M.lookup` m) neighbors =
    (S.union s $ S.fromList neighbors, x : xs)
  | otherwise = y
  where
    neighbors =
      [Coord (j - 1) i, Coord (j + 1) i, Coord j (i - 1), Coord j (i + 1)]

part1 :: [[Int]] -> Int
part1 xs =
  sum $ map ((+ 1) . (m M.!)) $ snd $ foldr (check m) (S.empty, []) (M.keys m)
  where
    m = intoMap xs

inject :: ([[Int]] -> Int) -> String -> String
inject f = show . f . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1])
