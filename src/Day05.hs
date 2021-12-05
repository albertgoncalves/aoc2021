import Data.Char (isDigit)
import Data.Map (Map, elems, fromListWith)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch1,
    readP_to_S,
    skipSpaces,
    string,
  )
import Prelude hiding (lines, span)

data Coord = Coord Int Int
  deriving (Eq, Ord)

data Line = Line Coord Coord

integer :: ReadP Int
integer = read <$> munch1 isDigit

coord :: ReadP Coord
coord = Coord <$> (integer <* char ',') <*> integer

line :: ReadP Line
line = Line <$> (coord <* skipSpaces <* string "->" <* skipSpaces) <*> coord

lines :: ReadP [Line]
lines = many1 (line <* skipSpaces)

parse :: String -> [Line]
parse = fst . head . readP_to_S (lines <* eof)

span :: Int -> Int -> [Int]
span x0 x1
  | x0 < x1 = [x0 .. x1]
  | x1 < x0 = [x0, (x0 - 1) .. x1]
  | otherwise = repeat x0

intoMap :: [Coord] -> Map Coord Int
intoMap xs = fromListWith (+) $ zip xs (repeat 1)

part1 :: Line -> [Coord]
part1 (Line (Coord x0 y0) (Coord x1 y1))
  | x0 == x1 = map (Coord x0) $ span y0 y1
  | y0 == y1 = map (`Coord` y0) $ span x0 x1
  | otherwise = []

part2 :: Line -> [Coord]
part2 (Line (Coord x0 y0) (Coord x1 y1)) =
  zipWith Coord (span x0 x1) (span y0 y1)

inject :: (Line -> [Coord]) -> String -> String
inject f = show . length . filter (1 <) . elems . intoMap . concatMap f . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, part2])
