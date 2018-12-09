module Problems.Eight (
    part1,
    part2
) where

import Data.Maybe (catMaybes)

data Node =
    Node {
        children:: [Node],
        metadata:: [Int]
    } deriving Show

parseNode ::
    [Int]
    -> (Node, [Int])
parseNode (0:metadataLength:contents) = (Node [] (take metadataLength contents), drop metadataLength contents)
parseNode (childCount:metadataLength:contents) = let
    (firstChild, xs) = parseNode contents
    (children, rest) = foldl parseChildNode ([firstChild], xs) [1..childCount-1]
    in (Node children (take metadataLength rest), drop metadataLength rest)
    where
        parseChildNode (acc, xs) _ = let
            (child, rest) = parseNode xs
            in (acc<>[child], rest)

metadataSum ::
    Node
    -> Int
metadataSum (Node [] md) = sum md
metadataSum (Node children md) = sum (metadataSum <$> children) + sum md

value ::
    Node
    -> Int
value (Node [] md) = sum md
value (Node children md) =
    sum . catMaybes $ foldr processMd [] md
    where
        processMd n acc = (value <$> safelyIndex children (n -1)) : acc
        safelyIndex xs n
            | n < 0 = Nothing
            | n >= length xs = Nothing
            | otherwise = Just . head $ drop n xs

part1 :: IO ()
part1 = do
    nums <- fmap read . words <$> readFile "data/8_1"
    --print . metadataSum . fst $ parseNode [2,3,0,3,10,11,12,1,1,0,1,99,2,1,1,2]
    print . metadataSum . fst $ parseNode nums

part2 :: IO ()
part2 = do
    nums <- fmap read . words <$> readFile "data/8_1"
    print . value . fst $ parseNode [2,3,0,3,10,11,12,1,1,0,1,99,2,1,1,2]
    print . value . fst $ parseNode nums
