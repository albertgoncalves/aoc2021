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

paths ::
  (M.Map Node Int -> S.Set Node -> S.Set Node) ->
  Graph ->
  M.Map Node Int ->
  Node ->
  [[Node]]
paths _ _ _ End = [[End]]
paths f g m n =
  concatMap (map (n :) . paths f g m') $
    S.toList $
      f m' $
        fromMaybe S.empty $
          M.lookup n g
  where
    m' = if isSmall n then M.insertWith (+) n 1 m else m

part1 :: S.Set Node -> M.Map Node Int -> S.Set Node
part1 s = S.difference s . S.fromList . M.keys

part2 :: M.Map Node Int -> S.Set Node -> S.Set Node
part2 m s
  | elem 2 $ M.elems m = S.difference s (S.fromList $ M.keys m)
  | otherwise = s

inject :: (M.Map Node Int -> S.Set Node -> S.Set Node) -> String -> String
inject f = show . length . (\g -> paths f g M.empty Start) . graph . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [flip part1, part2])
