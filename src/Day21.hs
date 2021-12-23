import Data.Char (isDigit)
import Data.List (foldl')
import Data.Map (Map, empty, insert, lookup)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    munch1,
    readP_to_S,
    string,
  )
import Prelude hiding (lookup)

newtype Score = Score {getScore :: Int}
  deriving (Eq, Ord)

newtype Position = Position {getPosition :: Int}
  deriving (Eq, Ord)

data Player = Player Position Score

type Memo = Map (Position, Position, Score, Score) (Integer, Integer)

newline :: ReadP Char
newline = char '\n'

integer :: ReadP Int
integer = read <$> munch1 isDigit

player :: Char -> ReadP Int
player x =
  string "Player " *> char x *> string " starting position: " *> integer

players :: ReadP (Int, Int)
players = (,) <$> (player '1' <* newline) <*> (player '2' <* newline)

parse :: String -> (Int, Int)
parse = fst . head . readP_to_S (players <* eof)

step :: Position -> Score -> Int -> (Position, Score)
step p s n = (Position p', Score $ getScore s + p')
  where
    p' = ((n + getPosition p - 1) `mod` 10) + 1

part1 :: Int -> Int -> Integer
part1 l r = toInteger $ getScore s' * length xs * 3
  where
    xs =
      takeWhile (\(_, _, Player _ s) -> getScore s < 1000) $
        iterate
          game
          ( cycle [1 .. 100],
            Player (Position l) (Score 0),
            Player (Position r) (Score 0)
          )
    (_, _, Player _ s') = last xs

    game :: ([Int], Player, Player) -> ([Int], Player, Player)
    game (xs', Player lP lS, Player rP rS) =
      (xs1, Player rP rS, uncurry Player $ step lP lS (sum xs0))
      where
        (xs0, xs1) = splitAt 3 xs'

dice :: [Int]
dice = [a + b + c | a <- [1 .. 3], b <- [1 .. 3], c <- [1 .. 3]]

-- NOTE: See `https://github.com/jonathanpaulson/AdventOfCode/blob/master/2021/21.py`.
memo ::
  Memo ->
  Position ->
  Position ->
  Score ->
  Score ->
  (Memo, (Integer, Integer))
memo m0 p0 p1 s0 s1
  | 21 <= getScore s0 = (m0, (1, 0))
  | 21 <= getScore s1 = (m0, (0, 1))
  | otherwise = case lookup (p0, p1, s0, s1) m0 of
    Just x -> (m0, x)
    Nothing -> (insert (p0, p1, s0, s1) x' m3, x')
  where
    f (m1, (l, r)) n = (m2, (l + r', r + l'))
      where
        (p0', s0') = step p0 s0 n
        (m2, (l', r')) = memo m1 p1 p0' s1 s0'
    (m3, x') = foldl' f (m0, (0, 0)) dice

part2 :: Int -> Int -> Integer
part2 l r =
  uncurry max $ snd $ memo empty (Position l) (Position r) (Score 0) (Score 0)

inject :: (Int -> Int -> Integer) -> String -> String
inject f = show . uncurry f . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, part2])
