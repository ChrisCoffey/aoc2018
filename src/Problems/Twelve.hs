module Problems.Twelve (
    part1,
    part2
    ) where

import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Text.Megaparsec
import Text.Megaparsec.Char (eol, digitChar, spaceChar, char, string)

data Planted = Plant | Empty
    deriving (Eq, Show, Ord)

newtype Pots = Pots [Planted]
    deriving (Show)

type Cell = (Planted, Planted, Planted, Planted, Planted)

newtype Rule = Rule (Cell, Planted)
    deriving (Eq, Show, Ord)

extendEdges ::
    Pots
    -> Pots
extendEdges (Pots pots) =
    Pots $ (Empty:Empty:Empty:pots)<> [Empty,Empty,Empty]

runGeneration ::
    [Rule]
    -> Pots
    -> Pots
runGeneration rules pots =
    Pots . go . unPots $ extendEdges pots
    where
        unPots (Pots p) = p
        go xs@(a:b:curr:d:e:[]) = step xs:[]
        go xs@(a:b:curr:d:e:rest) = step xs:(go (b:curr:d:e:rest))
        step (a:b:curr:d:e:_) =
            case catMaybes $ runRule (a,b,curr,d,e) <$> rules of
                [x] -> x
                [] -> curr

runRule ::
    Cell
    -> Rule
    -> Maybe Planted
runRule cell (Rule (state, output))
    | state == cell = Just output
    | otherwise = Nothing

part1 :: IO ()
part1 = do
    ls <- readFile "data/12.data"
    let Just (initialState, rules) = input ls
        finalState = foldr (f rules) initialState ([1..20::Int])
        size = length $ unPots initialState
        res = calcRes $ unPots finalState
    print size
    print finalState
    print (length $ unPots finalState)
    print res
        where
            unPots (Pots p) = p
            f rules _ pots = runGeneration rules pots
            calcRes = sum . fmap fst . filter ((== Plant) . snd) . zip [-20..]

part2 :: IO ()
part2 = do
    ls <- readFile "data/12.data"
    let Just (initialState, rules) = input ls
    -- runStep rules initialState 1
    print $ ((50000000000-99)*78) + 10189
        where
            runStep rules prevSt 3000 = do
                let tick = runGeneration rules prevSt
                    res = calcRes 3000 $ unPots tick
                    pRes = calcRes 2999 $ unPots prevSt
                print (res, res - pRes, 3000)
            runStep rules prevSt n = do
                let tick = runGeneration rules prevSt
                    res = calcRes n $ unPots tick
                    pRes = calcRes (n-1) $ unPots prevSt
                print (res, res - pRes, n)
                runStep rules tick (n+1)
            unPots (Pots p) = p
            calcRes n = sum . fmap fst . filter ((== Plant) . snd) . zip [(-n)..]


input :: String -> Maybe (Pots, [Rule])
input = parseMaybe inputParser

inputParser :: Parsec () String (Pots, [Rule])
inputParser = do
    pots <- stateParser
    _ <- many eol
    rules <- ruleParser `sepEndBy` eol
    pure (pots, rules)

stateParser :: Parsec () String Pots
stateParser = do
    _ <- string "initial state: "
    Pots <$> many plantedParser

ruleParser :: Parsec () String Rule
ruleParser = do
    a <- plantedParser
    b <- plantedParser
    c <- plantedParser
    d <- plantedParser
    e <- plantedParser
    _ <- string " => "
    output <- plantedParser
    pure $ Rule ((a,b,c,d,e), output)

plantedParser :: Parsec () String Planted
plantedParser =
    p <|> e
    where
        p = pure Plant <* char '#'
        e = char '.' *> pure Empty
