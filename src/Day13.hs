import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Set (Set, partition)
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
  deriving (Show)

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

fold :: Fold -> Set Coord -> Set Coord
fold (Fold Horizontal x) s = undefined
  where
    (l, r) = partition (\(Coord x' _) -> x' < x) s
fold (Fold Vertical y) s = undefined
  where
    (l, u) = partition (\(Coord _ y') -> y' < y) s

main :: IO ()
main = interact (show . parse)
