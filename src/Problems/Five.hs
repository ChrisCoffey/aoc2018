module Problems.Five (
    part1,
    part2
) where

import Data.Char (toLower)

part1 :: IO ()
part1 = do
    input <- readFile "data/5_1"
    print . length $ reducePolymers input

reducePolymers :: String -> String
reducePolymers xs = let
    xs' = inputPass xs
    in if length xs == length xs'
        then xs
        else reducePolymers xs'
    where
        inputPass [x] = [x]
        inputPass (a:b:rest)
            | sameTypeOppPolarity a b = inputPass rest
            | otherwise = a: inputPass (b:rest)

part2 :: IO ()
part2 = do
    input <- readFile "data/5_1"
    let reducedInputs = (`remType` input) <$> lowers
        results = length . reducePolymers <$> reducedInputs
    print $ minimum results
    where
        remType c [] = []
        remType c (a:rest)
            | toLower a == c = remType c rest
            | otherwise = a : remType c rest


lowers :: [Char]
lowers = ['a'..'z']

uppers :: [Char]
uppers = ['A'..'Z']

sameTypeOppPolarity ::
    Char
    -> Char
    -> Bool
sameTypeOppPolarity l r =
    toLower l == toLower r &&
    (
        (l `elem` lowers && r `elem` uppers) ||
        ( r `elem` lowers && l `elem` uppers )
    )
