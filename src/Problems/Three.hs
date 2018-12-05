module Problems.Three (
    part1,
    part2
) where

import Numeric.Natural
import Text.Megaparsec
import Text.Megaparsec.Char (eol, digitChar, spaceChar, char, string)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M

part1 :: IO ()
part1 = do
    raw <- TIO.readFile "data/3_1"
    let (Just input) = parseInputList raw
        applied = foldr applySugs M.empty input
    print . length $ filter (> 1)$ M.elems applied

applySugs :: Claim -> Fabric -> Fabric
applySugs claim fabric = foldr (\p m -> M.insertWith (+) p 1 m) fabric $ claimPoints claim

claimPoints ::
    Claim
    -> [Point]
claimPoints (Claim _ (Point x y) w h) = [
    Point x' y'| x' <- [x..x+w-1],
                    y' <- [y, y-1.. y-h+1] ]


part2 :: IO ()
part2 = do
    raw <- TIO.readFile "data/3_1"
    let (Just input) = parseInputList raw
        applied = foldr applySugs M.empty input
        result = filter (noOverlaps applied) input
    print result

noOverlaps ::
    Fabric
    -> Claim
    -> Bool
noOverlaps fabric claim = all isOne pts
    where
        isOne :: Point -> Bool
        isOne = (== (1 :: Natural)) . ((M.!) fabric)
        pts = claimPoints claim

data Point =
    Point {
        x :: Natural,
        y :: Natural
    } deriving (Show, Eq, Ord)

data Claim =
    Claim {
        claimId :: Natural,
        startingCorner :: Point,
        width :: Natural,
        height :: Natural
    } deriving (Show)

-- Represents the 1000x1000 piece of fabric and the number of times each point is claimed
type Fabric = M.Map Point Natural

parseInputList ::
    T.Text
    -> Maybe [Claim]
parseInputList = parseMaybe inputListParser

inputListParser :: Parsec () T.Text [Claim]
inputListParser =
    claimParser `sepEndBy` eol

claimParser :: Parsec () T.Text Claim
claimParser = do
    _ <- char '#'
    n <- natParser
    _ <- string " @ "
    point <- pointParser
    _ <- char ':'
    _ <- spaceChar
    width <- natParser
    _ <- char 'x'
    height <- natParser
    pure $ Claim {
        claimId = n,
        startingCorner = point,
        width = width,
        height = height
        }

pointParser :: Parsec () T.Text Point
pointParser = do
    x <- natParser
    _ <- char ','
    y <- natParser
    pure $ Point {x = x, y = 1000 - y} -- because coordinates are an offset from the top


natParser :: Parsec () T.Text Natural
natParser = read <$> many digitChar
