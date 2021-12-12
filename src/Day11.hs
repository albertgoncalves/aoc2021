{-# LANGUAGE TupleSections #-}

import Data.Array (Array, accum, assocs, elems, listArray)
import Data.Char (isDigit, ord)
import Data.Ix (inRange, range)
import Data.Set (Set, empty, insert, notMember)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch1,
    readP_to_S,
  )

type Coord = (Int, Int)

digits :: ReadP [Int]
digits = map (subtract (ord '0') . ord) <$> munch1 isDigit <* char '\n'

parse :: String -> [[Int]]
parse = fst . head . readP_to_S (many1 digits <* eof)

bounds :: (Coord, Coord)
bounds = ((0, 0), (9, 9))

intoArray :: [[a]] -> Array Coord a
intoArray = listArray bounds . concat

neighbors :: Coord -> [Coord]
neighbors c@(x, y) =
  filter (\c' -> (c /= c') && inRange bounds c') $
    range ((x - 1, y - 1), (x + 1, y + 1))

aboveThreshold :: Int -> Bool
aboveThreshold = (9 <)

resolve :: Set Coord -> Array Coord Int -> Int -> (Array Coord Int, Int)
resolve s a n =
  case filter (`notMember` s) $
    map fst $
      filter (aboveThreshold . snd) $
        assocs a of
    [] -> (a, n)
    (c : _) ->
      (+ 1) <$> resolve (insert c s) (accum (+) a $ map (,1) $ neighbors c) n

reset :: Array Coord Int -> Array Coord Int
reset =
  listArray bounds . map (\x -> if aboveThreshold x then 0 else x) . elems

onFirst :: (a -> c) -> (a, b) -> (c, b)
onFirst f (l, r) = (f l, r)

step :: Array Coord Int -> Int -> (Array Coord Int, Int)
step a n =
  onFirst reset $ resolve empty (accum (+) a $ map (,1) $ range bounds) n

loop :: Array Coord Int -> [(Array Coord Int, Int)]
loop = iterate (uncurry step) . (,0)

part1 :: Array Coord Int -> Int
part1 = snd . (!! 100) . loop

part2 :: Array Coord Int -> Int
part2 = length . takeWhile (not . all (== 0) . elems) . map fst . loop

inject :: (Array Coord Int -> Int) -> String -> String
inject f = show . f . intoArray . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, part2])
