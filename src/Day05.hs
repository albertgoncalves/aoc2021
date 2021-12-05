import Data.Char (isDigit)
import Data.Map (Map, fromListWith, toList)
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
span x0 x1 = if x0 < x1 then [x0 .. x1] else [x1 .. x0]

intoCoords :: Line -> [Coord]
intoCoords (Line (Coord x0 y0) (Coord x1 y1))
  | x0 == x1 = map (Coord x0) $ span y0 y1
  | y0 == y1 = map (`Coord` y0) $ span x0 x1
  | otherwise = []

intoMap :: [Coord] -> Map Coord Int
intoMap xs = fromListWith (+) $ zip xs (repeat 1)

part1 :: [Line] -> Int
part1 = length . filter ((1 <) . snd) . toList . intoMap . concatMap intoCoords

inject :: ([Line] -> Int) -> String -> String
inject f = show . f . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1])
