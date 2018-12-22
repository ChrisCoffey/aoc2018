module Problems.Eleven (
    part1,
    part2
) where

import Data.List (sortOn)
import Data.Foldable (maximumBy, foldl')
import Data.Ord (comparing)
import qualified Data.Map as M

import Debug.Trace

newtype X = X Int
    deriving (Eq, Ord, Show)
newtype Y = Y Int
    deriving (Eq, Ord, Show)
newtype SerialNum = S Int
newtype Power = P Int
    deriving (Eq, Ord, Show)
newtype Size = Z Int
    deriving (Show)

powerSum ::
    Power
    -> Power
    -> Power
powerSum (P l) (P r) = P $ l + r

cellPower ::
    X
    -> Y
    -> SerialNum
    -> Power
cellPower (X x) (Y y) (S s) = P p5
    where
        rackID = x+10
        p1 = rackID * y
        p2 = p1 + s
        p3 = p2 * rackID
        p4 = (p3 `mod` 1000) `div` 100
        p5 = p4 - 5

gridPower ::
    Size
    -> SerialNum
    -> X
    -> Y
    -> Power
gridPower (Z size) s (X x) (Y y) = foldr powerSum (P 0) powers
    where
    sz = size -1
    points = [(X x', Y y')| x' <- [x..x+sz], y' <- [y..y+sz]]
    powers = (\ (x, y) -> cellPower x y s) <$> points

maxPower ::
    Int
    -> Int
    -> Int
    -> SerialNum
    -> (Power, (X, Y, Int))
maxPower maxSize width height s = maximumBy (comparing fst) $ grids
    where
    grids :: [(Power, (X,Y,Int))]
    grids = [largestForSize z | z <- [1..maxSize]]

    largestForSize :: Int -> (Power, (X,Y,Int))
    largestForSize z = maximumBy (comparing fst) $ [
        (gridPower (Z z) s x y, (x, y, z)) |
            let offset = z-1,
            x <- X <$> [1.. (width-offset)],
            y <- Y <$> [1.. (height-offset)]
        ]

part1 :: IO ()
part1 =  print $ maxPower 3 300 300 (S 18)

part2 :: IO ()
part2 = print $ maxPower 300 300 300 (S 3628)

type SummedAreaT = M.Map (X,Y) Power
summedAreaTable ::
    SerialNum
    -> SummedAreaT
summedAreaTable s =
    foldl' go (M.fromList rawCellValues) $ [(X x, Y y) | x <- [1..300], y <- [1..300]]
    where
        go acc pt@(X x, Y y) = let
            upperCell = if y == 1
                        then P 0
                        else acc M.! (X x, Y ( y-1 ))
            diagonal = if y == 1 || x == 1
                        then P 0
                        else invertP $ acc M.! (X (x -1), Y ( y-1 ))
            leftCell = if x == 1
                       then P 0
                       else acc M.! (X (x -1), Y  y)
            subWeightSum = (((acc M.! pt) `powerSum` upperCell) `powerSum` leftCell) `powerSum` diagonal
            in (M.insertWith powerSum pt subWeightSum acc)

        rawCellValues = [((x,y), cellPower x y s)| x <- X <$> [1..300], y <- Y <$> [1..300]]

maxPower' ::
    SummedAreaT
    -> (Power, (X, Y, Int))
maxPower' table =
    maximumBy (comparing fst) $ [(computePower x y sz, (X x, Y y, sz))|
        sz <- [1..30],
        x <- [1..300-sz],
        y <- [1..300-sz]
      ]
    where
        computePower x y sz = let
            a = table M.! (X x, Y y)
            b = table M.! (X (x+sz), Y y)
            c = table M.! (X x, Y (y+sz))
            d = table M.! (X (x+sz), Y (y+sz))
            in ((powerSum a d) `powerSum` (invertP c)) `powerSum` (invertP b)

invertP ::
    Power
    -> Power
invertP (P n) = P (-n)
