import Data.Char (isDigit)
import Data.Function (on)
import Data.List (groupBy, sort, sortBy)
import Data.Map (Map, delete, fromList, keys, lookup)
import Data.Maybe (mapMaybe)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch,
    munch1,
    readP_to_S,
    sepBy1,
  )
import Prelude hiding (lookup, sequence)

type Board = Map Int (Int, Int)

data Game = Game [Int] [Board]

integer :: ReadP Int
integer = read <$> munch1 isDigit

spaces :: ReadP String
spaces = munch (== ' ')

newline :: ReadP Char
newline = char '\n'

newlines :: ReadP String
newlines = many1 newline

sequence :: ReadP [Int]
sequence = sepBy1 integer (char ',')

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

intoBoard :: [[Int]] -> Board
intoBoard rows =
  fromList [(x, (j, i)) | (j, row) <- enumerate rows, (i, x) <- enumerate row]

board :: ReadP Board
board = intoBoard <$> many1 (many1 (spaces *> integer) <* newline)

boards :: ReadP [Board]
boards = sepBy1 board newline

parse :: String -> Game
parse =
  fst . head . readP_to_S (Game <$> (sequence <* newlines) <*> (boards <* eof))

mark :: [Int] -> Board -> [(Int, Int)]
mark xs b = mapMaybe (`lookup` b) xs

isWinner :: [(Int, Int)] -> Bool
isWinner xs = elem 5 $ map length $ rows ++ cols
  where
    xs' = sort xs
    rows = groupBy ((==) `on` fst) xs'
    cols = groupBy ((==) `on` snd) $ sortBy (compare `on` snd) xs'

score :: [Int] -> Board -> Int
score xs b = sum (keys $ foldr delete b xs) * last xs

winners :: [Int] -> [Board] -> [(Board, [(Int, Int)])]
winners xs bs = filter (isWinner . snd) $ zip bs (map (mark xs) bs)

part1 :: Int -> Game -> Int
part1 n (Game xs bs) = head $ mapMaybe f [n ..]
  where
    f n' = case winners xs' bs of
      [(b, _)] -> Just $ score xs' b
      [] -> Nothing
      _ -> undefined
      where
        xs' = take n' xs

part2 :: Game -> Int
part2 (Game xs bs) = loop 1 bs
  where
    loop :: Int -> [Board] -> Int
    loop n bs'@[_] = part1 n (Game xs bs')
    loop n bs' =
      loop (n + 1) $
        map fst $
          filter (not . isWinner . snd) $
            zip bs' $
              map (mark (take n xs)) bs'

inject :: (Game -> Int) -> String -> String
inject f = show . f . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1 1, part2])
