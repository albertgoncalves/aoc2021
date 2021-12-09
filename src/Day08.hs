import Data.Bits (shift, (.|.))
import Data.Char (isLower)
import Data.List (delete, intersect, (\\))
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

ofLen :: Int -> [[a]] -> [[a]]
ofLen n = filter ((== n) . length)

ofLen1 :: Int -> [[a]] -> [a]
ofLen1 n = head . ofLen n

intersect1 :: Eq a => [a] -> [a] -> a
intersect1 xs = head . intersect xs

delete1 :: Eq a => a -> [a] -> a
delete1 x = head . delete x

decode :: Map Char Int -> String -> Int
decode m s =
  case foldr ((.|.) . shift 1 . (m !)) 0 s :: Int of
    119 -> 0
    36 -> 1
    93 -> 2
    109 -> 3
    46 -> 4
    107 -> 5
    123 -> 6
    37 -> 7
    127 -> 8
    111 -> 9
    _ -> undefined

part2 :: [String] -> [String] -> Int
part2 xs = foldl1 (\x -> ((x * 10) +)) . map (decode m)
  where
    l2 = ofLen1 2 xs
    l3 = ofLen1 3 xs
    l4 = ofLen1 4 xs
    l6 = ofLen 6 xs
    l7 = ofLen1 7 xs

    x0 = head $ l3 \\ l2
    x2 = l2 `intersect` l3
    x5 = x2

    x1 = delete x0 (l4 \\ l3)
    x3 = x1

    x4 = concatMap (l7 \\) l6
    x2' = intersect1 x2 x4
    x3' = intersect1 x3 x4

    x1' = delete1 x3' x1
    x4' = delete1 x2' $ delete x3' x4
    x5' = delete1 x2' x5
    x6 = head $ foldr delete "abcdefg" [x0, x1', x2', x3', x4', x5']

    m :: Map Char Int
    m =
      fromList
        [(x0, 0), (x1', 1), (x2', 2), (x3', 3), (x4', 4), (x5', 5), (x6, 6)]

inject :: (([String], [String]) -> Int) -> String -> String
inject f = show . sum . map f . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, uncurry part2])
