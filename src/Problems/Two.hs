module Problems.Two (
    part1,
    part2
) where

import Data.List (sort, group)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

part1 :: IO ()
part1 = do
    contents <- lines <$> readFile "data/2_1"
    let twos = filter (hasLetterCount 2) contents
        threes = filter (hasLetterCount 3) contents
    print $ length twos * length threes

hasLetterCount :: Int -> String -> Bool
hasLetterCount n = any ((==) n . length). group . sort

part2 :: IO ()
part2 = do
    contents <- lines <$> readFile "data/2_1"
    let possibilities = [(a,b)| a <- contents, b <- contents, a /= b, jaccard a b > 0.92]
        (a,b) = head possibilities
    print $ removeDifferentChars a b

jaccard ::
    String
    -> String
    -> Double
jaccard xs ys = let
    xBigrams = S.fromList (bigrams xs)
    yBigrams = S.fromList (bigrams ys)
    inter = S.intersection xBigrams yBigrams
    union = S.union xBigrams yBigrams
    in fromIntegral (S.size inter) / fromIntegral (S.size union)

removeDifferentChars ::
    String
    -> String
    -> String
removeDifferentChars [] _ = []
removeDifferentChars _ [] = []
removeDifferentChars (x:xs) (y:ys)
    | x == y = x: removeDifferentChars xs ys
    | otherwise = removeDifferentChars xs ys

data Bigram = B Char Char
    deriving (Eq, Ord)
bigrams ::
    String
    -> [Bigram]
bigrams xs = toBigram <$> zip (tail xs) xs
    where
        toBigram (a,b) = B a b
