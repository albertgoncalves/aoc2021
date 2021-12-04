import Data.Array (Array, elems, listArray, (!))
import Data.Bits (shift, (.|.))
import Data.Char (isDigit)
import Data.List (foldl')
import Text.ParserCombinators.ReadP
  ( ReadP,
    eof,
    munch1,
    readP_to_S,
    sepBy1,
    skipSpaces,
  )
import Prelude hiding (flip)

read' :: Char -> Int
read' '0' = 0
read' '1' = 1
read' _ = undefined

bits :: ReadP [Int]
bits = map read' <$> munch1 isDigit

parse :: String -> [[Int]]
parse = fst . head . readP_to_S (sepBy1 bits skipSpaces <* skipSpaces <* eof)

toDecimal :: [Int] -> Int
toDecimal = foldl' (\x -> (shift x 1 .|.)) 0

flip :: Int -> Int
flip 0 = 1
flip 1 = 0
flip _ = undefined

nearestAvg :: Int -> Int -> Int
nearestAvg n x =
  if ((fromIntegral x :: Float) / fromIntegral n) < 0.5
    then 0
    else 1

part1 :: [[Int]] -> Int
part1 xs = toDecimal ys * toDecimal (map flip ys)
  where
    ys = map (nearestAvg (length xs)) $ foldr1 (zipWith (+)) xs

toArray :: [a] -> Array Int a
toArray xs = listArray (0, length xs - 1) xs

loop :: (Int -> Int -> Bool) -> Int -> [Array Int Int] -> Int
loop _ _ [x] = toDecimal $ elems x
loop f i xs = loop f (i + 1) $ filter (\x -> f (x ! i) y) xs
  where
    y = nearestAvg (length xs) (sum $ map (! i) xs)

part2 :: [[Int]] -> Int
part2 xs = loop (==) 0 xs' * loop (/=) 0 xs'
  where
    xs' = map toArray xs

inject :: ([[Int]] -> Int) -> String -> String
inject f = show . f . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, part2])
