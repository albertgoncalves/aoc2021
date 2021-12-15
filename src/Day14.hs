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

run :: Ord a => Map (a, a) a -> [a] -> [a]
run _ [] = []
run _ [x] = [x]
run m (x0 : xs@(x1 : _)) =
  maybe [x0] (\x2 -> [x0, x2]) (lookup (x0, x1) m) ++ run m xs

histogram :: Ord a => [a] -> Map a Int
histogram xs = fromListWith (+) $ zip xs $ repeat 1

compute :: Int -> String -> Map (Char, Char) Char -> Int
compute n s m = maximum xs - minimum xs
  where
    xs = map snd $ assocs $ histogram $ (!! n) $ iterate (run m) s

inject :: Int -> String -> String
inject n = show . uncurry (compute n) . (fromList <$>) . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [10])
