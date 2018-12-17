module Problems.Ten (
    part1,
    part2
) where

import Data.Hashable
import Data.Foldable (traverse_, foldl', maximumBy, minimumBy, find)
import Data.List (sortOn, intercalate)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Control.Monad (foldM)
import qualified Data.HashSet as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char (eol, digitChar, spaceChar, char, string, anyChar)

-- | compute the intresting inputs, and extract the first one
part1 :: IO ()
part1 = do
    raw <- TIO.readFile "data/10_1"
    let input = fromMaybe (error "invalid input") $ parseMaybe parseInput raw
        res = fmap snd . find interestingInputs  $ (`tick` input) <$> [1..10245]
    putStrLn . fromMaybe "fail" $ prettyPrint <$> res
    where
        interestingInputs (_, st) =
            all (hasAdjacent st) st


part2 :: IO ()
part2 = do
    raw <- TIO.readFile "data/10_1"
    let input = fromMaybe (error "invalid input") $ parseMaybe parseInput raw
        res = fmap fst . find interestingInputs  $ (`tick` input) <$> [1..10245]
    print res
    where
        interestingInputs (_, st) =
            all (hasAdjacent st) st

hasAdjacent ::
    S.HashSet Point
    -> Point
    -> Bool
hasAdjacent cFilter (Point (x,y)) =
    any check neighbors
    where
        check point = S.member point cFilter
        neighbors = [Point (x, y+1),
                     Point (x, y-1),
                     Point (x+1, y),
                     Point (x-1, y),
                     Point (x-1,y-1),
                     Point (x+1,y+1),
                     Point (x+1, y-1),
                     Point (x-1, y+1)]

movePoint ::
    Int
    -> Input
    -> Point
movePoint time (Input (Point (x,y)) (Vector (dx,dy))) =
    Point (x+( time * dx),y+(time * dy))

tick ::
    Int
    -> [Input]
    -> (Int, S.HashSet Point)
tick time startingState = let
    points = movePoint time  <$> startingState
    in (time, S.fromList points)

    -- fold over the pionts & compute the next state


newtype Point = Point (Int, Int)
    deriving (Eq, Ord, Show, Hashable)

newtype Vector = Vector (Int, Int)
    deriving (Eq, Ord, Show, Hashable)

data Input = Input {
    point :: Point,
    vector ::  Vector
    } deriving (Eq, Show)

prettyPrint ::
    S.HashSet Point
    -> String
prettyPrint points =
    intercalate "\n" $ [[if S.member (Point (x,y)) points then 'X' else ' '| x <- [minX..maxX]]
        |y <- [minY..maxY]]
    where
        xV (Point (x,_)) = x
        yV (Point (_,y)) = y
        maxX = xV $ maximumBy (\(Point a) (Point b) -> comparing fst a b) points
        minX = xV $ minimumBy (\(Point a) (Point b) -> comparing fst a b) points
        maxY = yV $ maximumBy (\(Point a) (Point b) -> comparing snd a b) points
        minY = yV $ minimumBy (\(Point a) (Point b) -> comparing snd a b) points

parseInput :: Parsec () T.Text [Input]
parseInput =
    inputParser `sepEndBy` eol

inputParser :: Parsec () T.Text Input
inputParser = do
    _ <- string "position=<"
    p <- Point <$> parseNumPair
    _ <- many spaceChar
    _ <- string "velocity=<"
    v <- Vector <$> parseNumPair
    pure $ Input p v

parseNumPair :: Parsec () T.Text (Int, Int)
parseNumPair = do
    _ <- many spaceChar
    a <- read <$> manyTill anyChar (char ',')
    _ <- many spaceChar
    b <- read <$> manyTill anyChar (char '>')
    pure (a,b)
