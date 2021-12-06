import Data.Char (isDigit)
import Data.Map
  ( Map,
    elems,
    empty,
    foldrWithKey,
    fromList,
    insertWith,
    unionWith,
  )
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

intoMap :: [Int] -> Map Int Int
intoMap xs = foldr (uncurry (insertWith (+))) empty $ zip xs (repeat 1)

step :: Map Int Int -> Map Int Int
step = foldrWithKey f empty
  where
    f :: Int -> Int -> Map Int Int -> Map Int Int
    f k v m =
      unionWith (+) m $
        fromList $
          if k == 0
            then [(6, v), (8, v)]
            else [(k - 1, v)]

inject :: Int -> String -> String
inject n = show . sum . elems . (!! n) . iterate step . intoMap . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [80, 256])
