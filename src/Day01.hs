{-# LANGUAGE TypeApplications #-}

import Data.List (tails)

part1 :: [Int] -> [Int]
part1 = id

part2 :: [Int] -> [Int]
part2 = map sum . takeWhile ((n <=) . length) . map (take n) . tails
  where
    n :: Int
    n = 3

inject :: ([Int] -> [Int]) -> String -> String
inject f =
  show
    . length
    . filter (uncurry (<))
    . (\xs -> zip xs (tail xs))
    . f
    . map (read @Int)
    . lines

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, part2])
