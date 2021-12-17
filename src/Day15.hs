import Data.Array (Array, array, bounds, (!))
import Data.Char (isDigit, ord)
import Data.Ix (inRange)
import qualified Data.Map as M
import qualified Data.Set as S
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

zero :: Coord
zero = (0, 0)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

intoArray :: [[a]] -> Array Coord a
intoArray rows =
  array
    (zero, (x, y))
    [((j, i), v) | (i, row) <- enumerate rows, (j, v) <- enumerate row]
  where
    x = length (head rows) - 1
    y = length rows - 1

neighbors :: (Coord, Coord) -> Coord -> [Coord]
neighbors b (x, y) =
  filter (inRange b) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

distance :: Array Coord Int -> Int
distance a =
  search S.empty $
    M.fromListWith S.union $
      (0 :: Int, S.singleton zero) :
        [ (maxBound, S.singleton (j, i))
          | j <- [0 .. x],
            i <- [0 .. y],
            j /= 0 || i /= 0
        ]
  where
    b@(_, c@(x, y)) = bounds a

    search :: S.Set Coord -> M.Map Int (S.Set Coord) -> Int
    search s m =
      case M.minViewWithKey m of
        Nothing -> undefined
        Just ((n, cs), m') ->
          if S.member c cs
            then n
            else
              search s' $
                foldr
                  (\c' -> M.insertWith S.union (n + a ! c') (S.singleton c'))
                  m'
                  $ S.difference (S.fromList (concatMap (neighbors b) cs)) s'
          where
            s' = S.union s cs

increment :: Int -> Int -> Int
increment n x = ((x + n - 1) `mod` 9) + 1

tile :: Array Coord Int -> Int -> Int -> Int -> [(Coord, Int)]
tile a n u v =
  [((j + u', i + v'), increment n $ a ! (j, i)) | i <- [0 .. y], j <- [0 .. x]]
  where
    (_, (x, y)) = bounds a
    u' = (x + 1) * u
    v' = (y + 1) * v

expand :: Array Coord Int -> Array Coord Int
expand a =
  array (zero, (((x + 1) * n) - 1, ((y + 1) * n) - 1)) $
    concat [tile a (j + i) j i | i <- take n [0 ..], j <- take n [0 ..]]
  where
    n = 5 :: Int
    (_, (x, y)) = bounds a

inject :: (Array Coord Int -> Array Coord Int) -> String -> String
inject f = show . distance . f . intoArray . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [id, expand])
