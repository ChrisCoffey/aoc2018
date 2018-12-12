module Problems.Ten (
    part1,
    part2
) where

import Data.Hashable
import Data.CuckooFilter
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char (eol, digitChar, spaceChar, char, string, anyChar)

part1 :: IO ()
part1 = do
    raw <- TIO.readFile "data/10_1"
    let input = fromMaybe (error "invalid input") $ parseMaybe parseInput raw
        Just s = makeSize 1000000
        inputState = M.fromList $ (\(Input p v) -> ((p,v), p) ) <$> input
    filt <- initialize s :: IO (MFilter Point)
    res <- interestingInputs inputState filt 75000 []
    print $ length res
    where
        interestingInputs _ _ 0 acc = pure acc
        interestingInputs st filt n acc = do
            step <- tick st filt
            keep <- allM (hasAdjacent filt) (M.elems step)
            if keep
            then interestingInputs step filt (n-1) ((M.elems step):acc)
            else interestingInputs step filt (n-1) acc


part2 :: IO ()
part2 = undefined

allM :: Monad m =>
    (a -> m Bool)
    -> [a]
    -> m Bool
allM _ [] = pure True
allM pred (x:xs) = do
    b <- pred x
    if b
    then allM pred xs
    else pure b

hasAdjacent ::
    MFilter Point
    -> Point
    -> IO Bool
hasAdjacent cFilter (Point (x,y)) =
    foldM check False neighbors
    where
        check b point = (|| b) <$> member point cFilter
        neighbors = [Point (x, y+1),Point (x, y-1),Point (x+1, y),Point (x-1, y)]

movePoint ::
    Point
    -> (Point,Vector)
    -> MFilter Point
    -> IO ((Point,Vector), Point)
movePoint p@(Point (x,y)) key@(_,Vector (dx,dy)) cFilter = do
    cFilter' <- delete cFilter p
    resFilter <- insert cFilter next
    pure (key,next)
    where
        next = Point (x+dx,y+dy)

tick ::
    M.Map (Point,Vector) Point
    -> MFilter Point
    -> IO (M.Map (Point,Vector) Point)
tick currentState plot = do
    let xs = M.assocs currentState
    assocList <- traverse (\(v,p) -> movePoint p v plot) xs
    _ <- traverse_ (delete plot) $ fmap snd xs
    _ <- traverse_ (insert plot) $ fmap snd assocList
    pure $ M.fromList assocList


    -- fold over the pionts & compute the next state


newtype Point = Point (Int, Int)
    deriving (Eq, Ord, Show, Hashable)

newtype Vector = Vector (Int, Int)
    deriving (Eq, Ord, Show, Hashable)

data Input = Input {
    point :: Point,
    vector ::  Vector
    } deriving (Eq, Show)

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
