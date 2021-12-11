import Control.Monad (foldM)
import Data.Either (lefts, rights)
import Data.List (foldl', sort)

check :: String -> Either Char String
check = foldM f []
  where
    f :: String -> Char -> Either Char String
    f ('(' : xs) ')' = Right xs
    f ('[' : xs) ']' = Right xs
    f ('{' : xs) '}' = Right xs
    f ('<' : xs) '>' = Right xs
    f _ ')' = Left ')'
    f _ ']' = Left ']'
    f _ '}' = Left '}'
    f _ '>' = Left '>'
    f xs x = Right (x : xs)

part1 :: [String] -> Int
part1 = sum . map f . lefts . map check
  where
    f :: Char -> Int
    f ')' = 3
    f ']' = 57
    f '}' = 1197
    f '>' = 25137
    f _ = undefined

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` (2 :: Int))

part2 :: [String] -> Int
part2 =
  median
    . map (foldl' (\x -> ((x * 5) +)) 0 . map f)
    . rights
    . map check
  where
    f :: Char -> Int
    f '(' = 1
    f '[' = 2
    f '{' = 3
    f '<' = 4
    f _ = undefined

inject :: ([String] -> Int) -> String -> String
inject f = show . f . lines

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, part2])
