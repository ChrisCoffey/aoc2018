module Problems.One (
    part1,
    part2
) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S
import Numeric.Natural

import Debug.Trace

part1 :: IO ()
part1 = do
    operations <- fmap getOp . T.lines <$> TIO.readFile "data/1_1"
    print $ foldl applyOp 0 operations
    where
        applyOp acc (Add, n) = acc + n
        applyOp acc (Sub, n) = acc - n


data Op = Add | Sub
    deriving (Show)

getOp :: T.Text -> (Op, Int)
getOp t
    | T.null t = (Add, 0)
    | otherwise =
        case T.head t of
            '-' -> (Sub, read . T.unpack $ T.tail t)
            '+' -> (Add, read . T.unpack $ T.tail t)
            _ -> error "unknown operator"


part2 :: IO ()
part2 = do
    operations <- fmap getOp . T.lines <$> TIO.readFile "data/1_1"
    print . applyOp (T S.empty 0) . concat $ repeat operations
    where
        applyOp (T s acc) ((op, n):rest)
            | acc `S.member` s = acc
            | otherwise = let
                x = case op of
                    Add -> acc + n
                    Sub -> acc - n
                in applyOp (T (acc `S.insert` s) x) rest

data Tracked = T (S.Set Int) Int
