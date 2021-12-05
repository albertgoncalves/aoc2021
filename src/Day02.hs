import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
  ( ReadP,
    choice,
    eof,
    munch1,
    readP_to_S,
    sepBy1,
    skipSpaces,
    string,
  )

data Direction = Forward | Up | Down

data Move = Move Direction Int

data Position = Position Int Int Int

instance Semigroup Position where
  (Position x0 y0 z0) <> (Position x1 y1 z1) =
    Position (x0 + x1) (y0 + y1 + (x1 * z0)) (z0 + z1)

instance Monoid Position where
  mempty = Position 0 0 0

forward :: ReadP Direction
forward = Forward <$ string "forward"

up :: ReadP Direction
up = Up <$ string "up"

down :: ReadP Direction
down = Down <$ string "down"

direction :: ReadP Direction
direction = choice [forward, up, down]

integer :: ReadP Int
integer = read <$> munch1 isDigit

move :: ReadP Move
move = Move <$> (direction <* skipSpaces) <*> integer

moves :: ReadP [Move]
moves = sepBy1 move skipSpaces <* skipSpaces

parse :: String -> [Move]
parse = fst . head . readP_to_S (moves <* eof)

part1 :: Move -> Position
part1 (Move Forward n) = Position n 0 0
part1 (Move Up n) = Position 0 (- n) 0
part1 (Move Down n) = Position 0 n 0

part2 :: Move -> Position
part2 (Move Forward n) = Position n 0 0
part2 (Move Up n) = Position 0 0 (- n)
part2 (Move Down n) = Position 0 0 n

inject :: (Move -> Position) -> String -> String
inject f = show . (\(Position x y _) -> x * y) . mconcat . map f . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, part2])
