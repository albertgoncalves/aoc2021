import Control.Applicative ((<|>))
import Data.Bits (shift)
import Data.List (foldl')
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    count,
    eof,
    get,
    many,
    many1,
    readP_to_S,
    satisfy,
    string,
  )

data Packet
  = Literal Int
  | Eval ([Int] -> Int) [(Int, Packet)]

translate :: Char -> String
translate '0' = "0000"
translate '1' = "0001"
translate '2' = "0010"
translate '3' = "0011"
translate '4' = "0100"
translate '5' = "0101"
translate '6' = "0110"
translate '7' = "0111"
translate '8' = "1000"
translate '9' = "1001"
translate 'A' = "1010"
translate 'B' = "1011"
translate 'C' = "1100"
translate 'D' = "1101"
translate 'E' = "1110"
translate 'F' = "1111"
translate '\n' = ""
translate _ = undefined

fromBit :: Char -> Int
fromBit '0' = 0
fromBit '1' = 1
fromBit _ = undefined

decimal :: String -> Int
decimal = foldl' (\x -> (x `shift` 1 +) . fromBit) 0

unpack :: ReadP a -> String -> a
unpack p = fst . head . readP_to_S p

token :: ReadP Char
token = satisfy (`elem` "0123456789ABCDEF")

bit3 :: ReadP Int
bit3 = decimal <$> count 3 token

bits :: ReadP String
bits = count 4 token

literal :: ReadP Packet
literal = do
  _ <- string "100"
  xs <- many (char '1' *> bits)
  Literal . decimal . (concat xs ++) <$> (char '0' *> bits)

packet15 :: ReadP [(Int, Packet)]
packet15 = do
  _ <- char '0'
  n <- count 15 get
  unpack (many1 packet <* eof) <$> count (decimal n) get

packet11 :: ReadP [(Int, Packet)]
packet11 = do
  _ <- char '1'
  n <- count 11 get
  count (decimal n) packet

children :: ReadP [(Int, Packet)]
children = packet11 <|> packet15

sum' :: ReadP Packet
sum' = Eval sum <$> (string "000" *> children)

prod :: ReadP Packet
prod = Eval product <$> (string "001" *> children)

min' :: ReadP Packet
min' = Eval minimum <$> (string "010" *> children)

max' :: ReadP Packet
max' = Eval maximum <$> (string "011" *> children)

fromCompare :: (a -> a -> Bool) -> [a] -> Int
fromCompare f [l, r] = if f l r then 1 else 0
fromCompare _ _ = undefined

greaterThan :: ReadP Packet
greaterThan = Eval (fromCompare (>)) <$> (string "101" *> children)

lessThan :: ReadP Packet
lessThan = Eval (fromCompare (<)) <$> (string "110" *> children)

equal :: ReadP Packet
equal = Eval (fromCompare (==)) <$> (string "111" *> children)

packet :: ReadP (Int, Packet)
packet =
  (,) <$> bit3
    <*> ( literal
            <|> sum'
            <|> prod
            <|> min'
            <|> max'
            <|> greaterThan
            <|> lessThan
            <|> equal
        )

parse :: String -> (Int, Packet)
parse = unpack $ packet <* many (char '0') <* eof

part1 :: (Int, Packet) -> Int
part1 (n, Literal _) = n
part1 (n, Eval _ []) = n
part1 (n, Eval _ xs) = n + sum (map part1 xs)

part2 :: (Int, Packet) -> Int
part2 (_, Literal n) = n
part2 (_, Eval f xs) = f $ map part2 xs

inject :: ((Int, Packet) -> Int) -> String -> String
inject f = show . f . parse . concatMap translate

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, part2])
