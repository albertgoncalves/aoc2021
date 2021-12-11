import Data.Char (isDigit, ord)
import Data.List (sortBy)
import Data.Map (Map, fromList, keys, lookup, (!))
import Data.Maybe (mapMaybe)
import Data.Set (Set, empty, insert, member)
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

intoMap :: [[Int]] -> Map Coord Int
intoMap rows =
  fromList
    [(Coord j i, x) | (i, row) <- enumerate rows, (j, x) <- enumerate row]

neighbors :: Coord -> [Coord]
neighbors (Coord j i) =
  [Coord (j - 1) i, Coord (j + 1) i, Coord j (i - 1), Coord j (i + 1)]

isLowest :: Map Coord Int -> Coord -> Bool
isLowest m c = all (x <) $ mapMaybe (`lookup` m) (neighbors c)
  where
    x = m ! c

lowest :: Map Coord Int -> [Coord]
lowest m = filter (isLowest m) $ keys m

part1 :: Map Coord Int -> Int
part1 m = sum $ map ((+ 1) . (m !)) $ lowest m

higherNeighbors :: Map Coord Int -> Coord -> [Coord]
higherNeighbors m c =
  map fst $
    filter ((x <) . snd) $
      mapMaybe sequence $
        zip ns $
          map (`lookup` m) ns
  where
    ns = neighbors c
    x = m ! c

searchUp :: Set Coord -> Map Coord Int -> Coord -> (Set Coord, [Coord])
searchUp s m c
  | member c s = (s, [])
  | otherwise =
    case lookup c m of
      Nothing -> (s', [])
      Just 9 -> (s', [])
      _ ->
        foldr
          (\c'' (s'', cs) -> (cs ++) <$> searchUp s'' m c'')
          (s', [c])
          (higherNeighbors m c)
  where
    s' = insert c s

part2 :: Map Coord Int -> Int
part2 m =
  product $
    take 3 $
      sortBy (flip compare) $
        map (length . snd . searchUp empty m) $
          lowest m

inject :: (Map Coord Int -> Int) -> String -> String
inject f = show . f . intoMap . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, part2])
