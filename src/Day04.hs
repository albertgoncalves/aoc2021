import Data.Char (isDigit)
import Data.Map (Map, fromList)
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
import Prelude hiding (sequence)

type Board = Map Int (Int, Int)

data Game = Game [Int] [Board]
  deriving (Show)

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

game :: ReadP Game
game = Game <$> (sequence <* newlines) <*> (boards <* eof)

solve :: String -> String
solve = show . fst . head . readP_to_S game

main :: IO ()
main = interact solve
