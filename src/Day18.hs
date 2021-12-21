import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch1,
    readP_to_S,
  )

data Tree
  = Leaf Int
  | Node Tree Tree
  deriving (Show)

data Slice = Slice {getDepth :: Int, getValue :: Int}
  deriving (Show)

integer :: ReadP Int
integer = read <$> munch1 isDigit

leaf :: ReadP Tree
leaf = Leaf <$> integer

node :: ReadP Tree
node = Node <$> (char '[' *> tree <* char ',') <*> (tree <* char ']')

tree :: ReadP Tree
tree = leaf <|> node

parse :: String -> [Tree]
parse = fst . head . readP_to_S (many1 (tree <* char '\n') <* eof)

toSlices :: Int -> Tree -> [Slice]
toSlices n (Leaf x) = [Slice n x]
toSlices n (Node l r) = toSlices n' l ++ toSlices n' r
  where
    n' = n + 1

push :: Int -> [Slice] -> [Slice]
push _ [] = []
push x0 ((Slice n x1) : xs) = Slice n (x0 + x1) : xs

incr :: Slice -> Slice
incr (Slice n x) = Slice (n + 1) x

add :: [Slice] -> [Slice] -> [Slice]
add ls rs = map incr ls ++ map incr rs

explode :: [Slice] -> [Slice] -> Either [Slice] [Slice]
explode ls [] = Right $ reverse ls
explode ls [r] = Right $ reverse (r : ls)
explode ls (r0@(Slice n0 x0) : r1@(Slice n1 x1) : rs)
  | (n0 == n1) && (4 < n0) =
    Left $ reverse (push x0 ls) ++ (Slice (n0 - 1) 0 : push x1 rs)
  | n0 == n1 = explode (r1 : r0 : ls) rs
  | otherwise = explode (r0 : ls) (r1 : rs)

split :: [Slice] -> [Slice] -> Either [Slice] [Slice]
split ls [] = Right $ reverse ls
split ls (r@(Slice n x) : rs)
  | 9 < x =
    Left $ reverse ls ++ (Slice n' (floor x') : Slice n' (ceiling x') : rs)
  | otherwise = split (r : ls) rs
  where
    x' = fromIntegral x / (2.0 :: Float)
    n' = n + 1

reduce :: [Slice] -> [Slice]
reduce = either reduce (either reduce id . split []) . explode []

magnitude :: [Slice] -> [Slice] -> Int
magnitude [] [] = 0
magnitude [Slice _ x] [] = x
magnitude ls [] = magnitude [] $ reverse ls
magnitude ls ((Slice n0 x0) : (Slice n1 x1) : rs)
  | n0 == n1 =
    magnitude [] $ reverse ls ++ Slice (n0 - 1) (x0 * 3 + x1 * 2) : rs
magnitude ls (r : rs) = magnitude (r : ls) rs

part1 :: [[Slice]] -> Int
part1 = magnitude [] . foldl1 (\xs -> reduce . add xs)

part2 :: [[Slice]] -> Int
part2 xs =
  maximum $ [(magnitude [] . reduce . uncurry add) (l, r) | l <- xs, r <- xs]

inject :: ([[Slice]] -> Int) -> String -> String
inject f = show . f . map (toSlices 0) . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, part2])
