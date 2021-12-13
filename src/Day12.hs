import Control.Applicative ((<|>))
import Data.Char (isLower, isUpper)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many1,
    munch1,
    readP_to_S,
    string,
  )

data Node
  = Start
  | End
  | Small String
  | Large String
  deriving (Eq, Ord)

type Edge = (Node, Node)

type Graph = M.Map Node (S.Set Node)

start :: ReadP Node
start = Start <$ string "start"

end :: ReadP Node
end = End <$ string "end"

small :: ReadP Node
small = Small <$> munch1 isLower

large :: ReadP Node
large = Large <$> munch1 isUpper

node :: ReadP Node
node = start <|> end <|> small <|> large

edge :: ReadP Edge
edge = (,) <$> (node <* char '-') <*> node

parse :: String -> [Edge]
parse = fst . head . readP_to_S (many1 (edge <* char '\n') <* eof)

bothWays :: [(a, a)] -> [(a, a)]
bothWays xs = xs ++ map swap xs

graph :: [Edge] -> Graph
graph = M.fromListWith S.union . map (S.singleton <$>) . mapMaybe f . bothWays
  where
    f (_, Start) = Nothing
    f (End, _) = Nothing
    f x = Just x

isSmall :: Node -> Bool
isSmall (Small _) = True
isSmall _ = False

connectedTo :: Node -> Graph -> S.Set Node
connectedTo n g = fromMaybe S.empty $ n `M.lookup` g

from :: Node -> [[Node]] -> [[Node]]
from n = map (n :)

part1 :: Graph -> [[Node]]
part1 g = loop S.empty Start
  where
    loop :: S.Set Node -> Node -> [[Node]]
    loop _ End = [[End]]
    loop s n =
      concatMap (from n . loop s') $
        S.toList $
          S.difference (connectedTo n g) s
      where
        s' = if isSmall n then S.insert n s else s

trim :: M.Map Node Int -> S.Set Node -> S.Set Node
trim m s
  | elem 2 $ M.elems m = S.difference s (S.fromList $ M.keys m)
  | otherwise = s

part2 :: Graph -> [[Node]]
part2 g = loop M.empty Start
  where
    loop :: M.Map Node Int -> Node -> [[Node]]
    loop _ End = [[End]]
    loop m n =
      concatMap (from n . loop m') $ S.toList $ trim m' $ connectedTo n g
      where
        m' = if isSmall n then M.insertWith (+) n 1 m else m

inject :: (Graph -> [[Node]]) -> String -> String
inject f = show . length . f . graph . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, part2])
