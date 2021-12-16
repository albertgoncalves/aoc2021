import Data.Char (isUpper)
import Data.Map (Map, assocs, fromList, fromListWith, lookup)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    readP_to_S,
    satisfy,
    string,
  )
import Prelude hiding (lookup)

newline :: ReadP Char
newline = char '\n'

upper :: ReadP Char
upper = satisfy isUpper

chars :: ReadP String
chars = many1 upper <* newline

pair :: ReadP (Char, Char)
pair = (,) <$> upper <*> upper

rule :: ReadP ((Char, Char), Char)
rule = (,) <$> (pair <* string " -> ") <*> upper

rules :: ReadP [((Char, Char), Char)]
rules = many1 $ rule <* newline

parse :: String -> (String, [((Char, Char), Char)])
parse = fst . head . readP_to_S ((,) <$> (chars <* newline) <*> (rules <* eof))

merge :: (Ord a, Num b) => [(a, b)] -> Map a b
merge = fromListWith (+)

intoMap :: String -> Map (Char, Char) Int
intoMap s = merge $ zip (zip s' $ tail s') $ repeat 1
  where
    s' = " " ++ s ++ " "

step :: Map (Char, Char) Char -> (Char, Char) -> Int -> [((Char, Char), Int)]
step m x@(c0, c2) n =
  maybe [(x, n)] (\c1 -> [((c0, c1), n), ((c1, c2), n)]) (lookup x m)

run :: Map (Char, Char) Char -> [((Char, Char), Int)] -> [((Char, Char), Int)]
run m = assocs . merge . concatMap (uncurry $ step m)

unpack :: [((Char, Char), Int)] -> [(Char, Int)]
unpack = map (fmap (`div` 2)) . assocs . merge . concatMap (uncurry f)
  where
    f (' ', c) n = [(c, n)]
    f (c, ' ') n = [(c, n)]
    f (c0, c1) n = [(c0, n), (c1, n)]

compute :: Int -> String -> Map (Char, Char) Char -> Int
compute n s m = maximum xs - minimum xs
  where
    xs = map snd . unpack $ (!! n) $ iterate (run m) $ assocs $ intoMap s

inject :: Int -> String -> String
inject n = show . uncurry (compute n) . (fromList <$>) . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [10, 40])
