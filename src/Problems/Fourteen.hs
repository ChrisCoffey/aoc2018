module Problems.Fourteen (
    part1,
    part2
    ) where

import Data.Foldable (find)
import Data.List (findIndex)
import qualified Data.Map as M

import Debug.Trace

-- | Represents an elf's current recipe
newtype Elf = Elf Int
    deriving (Eq, Show, Ord)
newtype Score = Score Int
    deriving (Eq, Show, Ord, Num)
unScore (Score n) = n

type Recipes = M.Map Int Score

combine ::
    Recipes
    -> Elf
    -> Elf
    -> Recipes
combine scoreboard (Elf a) (Elf b) =
    M.union (M.fromList pairs) scoreboard
    where
        scoreboardLength = M.size scoreboard
        s1 = scoreboard M.! a
        s2 = scoreboard M.! b
        newScores = fmap (\c -> Score $ read [c]) . show $ unScore s1 + unScore s2 :: [Score]
        pairs = zip [scoreboardLength..] newScores

chooseNextRecipe ::
    Recipes
    -> Elf
    -> Elf
chooseNextRecipe scoreboard (Elf current) = Elf next
    where
        currentScore = unScore $ scoreboard M.! current
        scoreboardLength = M.size scoreboard
        next = (current+1+currentScore) `mod` scoreboardLength

tick ::
    Recipes
    -> Elf
    -> Elf
    -> (Recipes,Elf,Elf)
tick scoreboard elf1 elf2 =
    (scoreboard', elf1', elf2')
    where
        scoreboard' = combine scoreboard elf1 elf2
        elf1' = chooseNextRecipe scoreboard' elf1
        elf2' = chooseNextRecipe scoreboard' elf2

run ::
    Int
    -> Recipes
    -> [Score]
run terminateOn recipes =
    go 2 (recipes, Elf 0, Elf 1)
    where
        go n (rxs, e1, e2)
            | n == terminateOn + 10 = computeResult terminateOn rxs | otherwise =  go (n+1) $ tick rxs e1 e2

computeResult ::
    Int
    -> Recipes
    -> [Score]
computeResult n scoreboard =
    (scoreboard M.!) <$> [n..n+9]

runPart2 ::
    Recipes
    -> Maybe (String, String, Int)
runPart2 scoreboard =
    find check . fmap (\(a, _, _) -> prevFive a) $ iterate go (scoreboard, Elf 6, Elf 4)
    where
        go (rsx, e1, e2) = tick rsx e1 e2
        check (s,s', _) = s == "92510" || s' == "92510"

        prevFive rsx = let
            sz = M.size rsx
            scores = (rsx M.!) <$> [sz-5..(sz -1) ] :: [Score]
            scores' = (rsx M.!) <$> [sz-6..(sz -2) ] :: [Score]
            str = concat $ (show . unScore) <$> scores
            str' = concat $ (show . unScore) <$> scores'
            in (str, str', sz -5)

part1 :: IO ()
part1 = print $ unScore <$> run 920831 (M.fromList [(0,Score 3), (1, Score 7)])

part2 :: IO ()
part2 = print $ runPart2 (M.fromList [(0,Score 3), (1, Score 7), (2, Score 1), (3, Score 0), (4, Score 1), (5, Score 0), (6, Score 1)])
