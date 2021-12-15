import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.List (foldl', intercalate)
import qualified Data.Set as S
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch1,
    readP_to_S,
    string,
  )

data Coord = Coord Int Int
  deriving (Eq, Ord)

data Axis
  = Horizontal
  | Vertical
  deriving (Show)

data Fold = Fold Axis Int
  deriving (Show)

newline :: ReadP Char
newline = char '\n'

integer :: ReadP Int
integer = read <$> munch1 isDigit

coord :: ReadP Coord
coord = Coord <$> (integer <* char ',') <*> integer

coords :: ReadP [Coord]
coords = many1 $ coord <* newline

horizontal :: ReadP Fold
horizontal = Fold Horizontal <$> (string "fold along x=" *> integer)

vertical :: ReadP Fold
vertical = Fold Vertical <$> (string "fold along y=" *> integer)

folds :: ReadP [Fold]
folds = many1 $ (horizontal <|> vertical) <* newline

parse :: String -> ([Coord], [Fold])
parse =
  fst . head . readP_to_S ((,) <$> (coords <* newline) <*> (folds <* eof))

fold :: S.Set Coord -> Fold -> S.Set Coord
fold s (Fold Horizontal x) =
  S.union l $ S.map (\(Coord x' y) -> Coord (x - (x' - x)) y) r
  where
    (l, r) = S.partition (\(Coord x' _) -> x' < x) s
fold s (Fold Vertical y) =
  S.union l $ S.map (\(Coord x y') -> Coord x $ y - (y' - y)) r
  where
    (l, r) = S.partition (\(Coord _ y') -> y' < y) s

part1 :: [Coord] -> [Fold] -> String
part1 cs = show . length . foldl' fold (S.fromList cs) . take 1

part2 :: [Coord] -> [Fold] -> String
part2 cs fs =
  intercalate
    "\n"
    [ [if S.member (Coord x y) s then '.' else ' ' | x <- [0 .. 38]]
      | y <- [0 .. 5]
    ]
  where
    s = foldl' fold (S.fromList cs) fs

inject :: ([Coord] -> [Fold] -> String) -> String -> String
inject f = uncurry f . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, part2])
