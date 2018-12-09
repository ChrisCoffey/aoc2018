module Problems.Seven (
    part1,
    part2
) where

import Data.Char (ord)
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



data MultiDep = MD {mlabel:: String, timeLeft :: Int}
    deriving (Show)

data Step = S {
    time :: Int,
    inProgress :: [MultiDep],
    graph :: Graph,
    remainingDeps :: [Dep]
    } deriving (Show)

-- Track time remaining. Add dependencies to the output when they're found.
-- Time is tracked via an incrementing counter. Dependencies track time remaining
-- Multiple items may be started, so track an "in progress" queue
runStepsManyElves ::
    Step
    -> Step
runStepsManyElves step
    | null (inProgress step) && null (remainingDeps step) = step {time = time step + 84}
    | otherwise = runStepsManyElves $ step {
        time= time step + 1,
        inProgress = trace ((show (time step ))<> show (newSteps <> (tickTock <$> activeSteps))) $ (newSteps <> (tickTock <$> activeSteps)),
        remainingDeps = remainingSteps,
        graph = resultGraph
        }
    where
        -- immediately start processing the next steps
        activeSteps = filter (not . (==) 0. timeLeft) $ inProgress step
        completedSteps = fmap mlabel . filter ((==) 0. timeLeft) $ inProgress step
        -- Logs the steps that have completd this tick
        graph' = filter (not . flip elem completedSteps) <$> graph step

        tickTock md =  md {timeLeft = timeLeft md - 1}
        stepCountToStart = 5 - length activeSteps


        makeMultiDep :: Dep -> MultiDep
        makeMultiDep (Dep label _) = MD {
            mlabel = label,
            timeLeft = (\x -> x - 5) . ord $ head label
            }

        labelNub [] = []
        labelNub (x:xs) = x: labelNub (filter ((/= (label x)) . label) xs)
        -- Should immediatley include the steps
        nextSteps = labelNub . sort . filter (not . flip M.member resultGraph . label)  $ remainingSteps
        newSteps = makeMultiDep <$> take stepCountToStart nextSteps

        -- remaining steps are those that are not already completed and are present in remaining
        remainingSteps = filter (not . flip elem (mlabel <$> inProgress step). label) $ remainingDeps step
        terminalDeps = fmap (\x -> Dep x x) . sort . M.keys $ M.filter null graph'
        resultGraph = M.fromList . filter (not . null . snd) $ M.toList graph'


part1 :: IO ()
part1 = do
    input <- fmap parse . lines <$> readFile "data/7_1"
    let g = buildGraph input
    print . concat . fmap label $ runSteps input g

part2 :: IO ()
part2 = do
    input <- fmap parse . lines <$> readFile "data/7_1"
    let g = buildGraph input
        initialState = S {
            time = 0,
            inProgress = [],
            graph = g,
            remainingDeps = input
            }
    print $ runStepsManyElves initialState

