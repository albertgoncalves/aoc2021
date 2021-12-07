import Data.Char (isDigit)
import Data.List (partition)
import Text.ParserCombinators.ReadP
  ( char,
    eof,
    munch1,
    readP_to_S,
    sepBy1,
    skipSpaces,
  )

parse :: String -> [Int]
parse =
  fst
    . head
    . readP_to_S
      (sepBy1 (read <$> munch1 isDigit) (char ',') <* skipSpaces <* eof)

quickSelect :: Ord a => [a] -> Int -> a
quickSelect [] _ = undefined
quickSelect (x : xs) k
  | k < l = quickSelect a k
  | k > l = quickSelect b (k - l - 1)
  | otherwise = x
  where
    (a, b) = partition (< x) xs
    l = length a

dist :: Int -> Int -> Int
dist a b = abs $ a - b

part1 :: [Int] -> Int
part1 xs = sum $ map (dist (quickSelect xs $ length xs `div` 2)) xs

findMin :: (Int -> Int) -> Int -> Int
findMin f x
  | f l < y = findMin f l
  | f r < y = findMin f r
  | otherwise = y
  where
    y = f x
    l = x - 1
    r = x + 1

weight :: Int -> Int
weight n = sum [0 .. n]

part2 :: [Int] -> Int
part2 xs =
  findMin (sum . (\n -> map (weight . dist n) xs)) $
    round $
      (fromIntegral (sum xs) :: Float) / fromIntegral (length xs)

inject :: ([Int] -> Int) -> String -> String
inject f = show . f . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, part2])
