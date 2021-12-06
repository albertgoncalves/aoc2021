import Data.Char (isDigit)
import Data.IntMap (IntMap, assocs, elems, fromListWith)
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

merge :: [(Int, Int)] -> IntMap Int
merge = fromListWith (+)

step :: IntMap Int -> IntMap Int
step = merge . concatMap (uncurry f) . assocs
  where
    f :: Int -> Int -> [(Int, Int)]
    f k v
      | k == 0 = [(6, v), (8, v)]
      | otherwise = [(k - 1, v)]

inject :: Int -> String -> String
inject n =
  show
    . sum
    . elems
    . (!! n)
    . iterate step
    . merge
    . (`zip` repeat 1)
    . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [80, 256])
