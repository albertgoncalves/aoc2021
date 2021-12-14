import Control.Applicative ((<|>))
import Data.Char (isLower, isUpper)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
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
import Prelude hiding (lookup)

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

graph :: [Edge] -> Graph
graph xs =
  M.fromListWith S.union $
    map (S.singleton <$>) $
      filter (\x -> fst x /= End && snd x /= Start) $
        xs ++ map swap xs

isSmall :: Node -> Bool
isSmall (Small _) = True
isSmall _ = False

lookup :: Node -> Graph -> S.Set Node
lookup n = fromMaybe S.empty . M.lookup n

from :: Node -> [[Node]] -> [[Node]]
from n = map (n :)

part1 :: Graph -> [[Node]]
part1 g = loop S.empty Start
  where
    loop :: S.Set Node -> Node -> [[Node]]
    loop _ End = [[End]]
    loop s n =
      concatMap (from n . loop s') $ S.toList $ S.difference (lookup n g) s
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
      concatMap (from n . loop m') $ S.toList $ trim m' $ lookup n g
      where
        m' = if isSmall n then M.insertWith (+) n 1 m else m

inject :: (Graph -> [[Node]]) -> String -> String
inject f = show . length . f . graph . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, part2])
