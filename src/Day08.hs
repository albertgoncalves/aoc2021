import Data.Char (isLower)
import Data.Function (on)
import Data.List (sort, sortBy)
import Data.Map (Map, fromList, (!))
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch1,
    readP_to_S,
    sepBy1,
  )
import Prelude hiding (words)

space :: ReadP Char
space = char ' '

words :: ReadP [String]
words = sepBy1 (munch1 isLower) space

line :: ReadP ([String], [String])
line = (,) <$> (words <* space <* char '|' <* space) <*> words

parse :: String -> [([String], [String])]
parse = fst . head . readP_to_S (many1 (line <* char '\n') <* eof)

part1 :: ([String], [String]) -> Int
part1 = length . filter (`elem` [2, 3, 4, 7]) . map length . snd

segments :: [String]
segments =
  [ "abcefg",
    "cf",
    "acdeg",
    "acdfg",
    "bcdf",
    "abdfg",
    "abdefg",
    "acf",
    "abcdefg",
    "abcdfg"
  ]

reference :: Map String Int
reference = fromList $ zip segments [0 .. 9]

hash :: Char -> [String] -> [Int]
hash x xs =
  map snd $
    filter (elem x . fst) $
      sortBy (compare `on` snd) $
        zip xs (map length xs)

keys :: String
keys = "abcdefg"

encoder :: [String] -> Map Char [Int]
encoder xs = fromList $ map (\x -> (x, hash x xs)) keys

decoder :: Map [Int] Char
decoder = fromList $ map (\x -> (hash x segments, x)) keys

translate :: [String] -> String -> Int
translate xs = ((reference !) . sort) . map ((decoder !) . (encoder xs !))

-- NOTE: See `https://www.reddit.com/r/haskell/comments/rbj981/advent_of_code_2021_day_08/hnpbpv2/`.
part2 :: [String] -> [String] -> Int
part2 xs = foldl1 (\x -> ((x * 10) +)) . map (translate xs)

inject :: (([String], [String]) -> Int) -> String -> String
inject f = show . sum . map f . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, uncurry part2])
