import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    munch1,
    option,
    readP_to_S,
    string,
  )

data Range = Range Int Int

data Bounds = Bounds Range Range

data Speed = Speed Int Int

data Coord = Coord {getX :: Int, getY :: Int}

integer :: ReadP Int
integer = (\xs -> read . (xs ++)) <$> option "" (string "-") <*> munch1 isDigit

range :: ReadP Range
range = Range <$> (integer <* string "..") <*> integer

ranges :: ReadP Bounds
ranges =
  Bounds <$> (string "target area: x=" *> range) <*> (string ", y=" *> range)

parse :: String -> Bounds
parse = fst . head . readP_to_S (ranges <* char '\n' <* eof)

step :: Coord -> Speed -> (Coord, Speed)
step (Coord x y) (Speed dX dY) = (Coord (x + dX) (y + dY), Speed dX' (dY - 1))
  where
    dX'
      | dX < 0 = dX + 1
      | 0 < dX = dX - 1
      | otherwise = 0

run :: Bounds -> [Coord] -> Coord -> Speed -> [Coord]
run b@(Bounds (Range lX rX) (Range lY rY)) cs c@(Coord x y) s@(Speed dX _)
  | y < lY = []
  | (x < lX) && (dX <= 0) = []
  | (rX < x) && (0 <= dX) = []
  | (lX <= x) && (x <= rX) && (lY <= y) && (y <= rY) = cs'
  | otherwise = uncurry (run b cs') (step c s)
  where
    cs' = c : cs

possibleX :: Range -> [Int]
possibleX (Range lX rX)
  | (lX < 0) && (rX < 0) = undefined
  | rX < lX = undefined
  | otherwise = [dX | dX <- [1 .. rX], f dX]
  where
    f dX =
      any
        ((\x -> lX <= x && x <= rX) . fst)
        ( takeWhile ((0 <=) . snd) $
            iterate (\(x, dX') -> (x + dX', dX' - 1)) (0, dX)
        )

possibleY :: Range -> [Int]
possibleY (Range lY rY)
  | rY < lY = undefined
  | otherwise = [dY | dY <- [lY .. abs lY], f dY]
  where
    f dY =
      any
        ((\y -> lY <= y && y <= rY) . fst)
        ( takeWhile ((lY <=) . fst) $
            iterate (\(y, dY') -> (y + dY', dY' - 1)) (0, dY)
        )

part1 :: Bounds -> Int
part1 b@(Bounds rX rY) =
  maximum $
    concat $
      [ map getY $ run b [] (Coord 0 0) (Speed dX dY)
        | dX <- possibleX rX,
          dY <- possibleY rY
      ]

part2 :: Bounds -> Int
part2 b@(Bounds rX rY) =
  length $
    [ (dX, dY)
      | dX <- possibleX rX,
        dY <- possibleY rY,
        not $ null $ run b [] (Coord 0 0) (Speed dX dY)
    ]

inject :: (Bounds -> Int) -> String -> String
inject f = show . f . parse

main :: IO ()
main = interact (\x -> unlines $ map (`inject` x) [part1, part2])
