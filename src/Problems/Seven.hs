module Problems.Seven (
    part1,
    part2
) where

import Data.List (sort)
import qualified Data.Map as M

import Debug.Trace

data Dep = Dep {label :: String, upstream :: String}
    deriving (Show, Eq)

instance Ord Dep where
 (<=) l r = label l <= label r

parse ::
    String
    -> Dep
parse str =
    case words str of
        [_, label, _, _, _, _, _, upstream, _, _] -> Dep label upstream

-- Graph = Node -> Dependencies
type Graph = M.Map String [String]

buildGraph ::
    [Dep]
    -> Graph
buildGraph = foldl build M.empty
    where
        build acc (Dep lbl up) = M.insertWith (<>) up [lbl] acc

runSteps ::
    [Dep]
    -> Graph
    -> [Dep]
runSteps deps graph
    | length deps == 1 = nextStep:terminalDeps
    | otherwise = nextStep : (runSteps remainingSteps resultGraph)
    where
    nextSteps = sort $ filter (not . flip M.member graph . label) deps
    nextStep = head nextSteps
    remainingSteps = filter ((/=) (label nextStep) . label) deps
    graph' = filter (/= label nextStep) <$> graph
    terminalDeps = fmap (\x -> Dep x x) . sort . M.keys $ M.filter null graph'
    resultGraph = M.fromList . filter (not . null . snd) $ M.toList graph'


-- Track time remaining. Add dependencies to the output when they're found.
-- Time is tracked via an incrementing counter. Dependencies track time remaining
-- Multiple items may be started, so track an "in progress" queue
runStepsManyElves ::
    [Dep]
runStepsManyElves

part1 :: IO ()
part1 = do
    input <- fmap parse . lines <$> readFile "data/7_1"
    let g = buildGraph input
    print . concat . fmap label $ runSteps input g

part2 :: IO ()
part2 = do
    input <- fmap parse . lines <$> readFile "data/7_1"
    print input

