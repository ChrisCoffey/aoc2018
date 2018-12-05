module Problems.Four (
    part1,
    part2
) where

import Data.List (sortOn)
import Numeric.Natural
import Text.Megaparsec
import Text.Megaparsec.Char (eol, digitChar, spaceChar, char, string)
import Data.Foldable (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M

part1 :: IO ()
part1 = do
    raw <- TIO.readFile "data/4_1"
    let (Just input) = parseInputList raw
        xs = computeReports (M.empty, -1) input
        sleepiest = sleepiestGuard xs
        sleepMin = sleepiestMinute$ xs M.! sleepiest
    print sleepiest
    print sleepMin
    print $ xs M.! sleepiest
    print $ sleepiest * sleepMin
    where
        computeReports ::  (M.Map Natural MidnightHour, Natural) -> [(TS,Event)] -> M.Map Natural MidnightHour
        computeReports (acc, gid) [] = acc
        computeReports (acc, gid) ((_, GuardStart gid'):rest) = computeReports (acc, gid') rest
        computeReports (acc, gid) ((ts1, Asleep):(ts2, Awake):rest) = let
            MH mins = fromMaybe emptyMidnight $ M.lookup gid acc
            updated = foldr (\x m -> M.insertWith (+) x 1 m) mins [minute ts1.. (minute ts2) -1]
            in computeReports (M.insert gid (MH updated) acc, gid) rest

        sleepiestGuard :: M.Map Natural MidnightHour -> Natural
        sleepiestGuard = fst . maximumBy (comparing (minutesAsleep . snd)) . M.toList


part2 :: IO ()
part2 = do
    raw <- TIO.readFile "data/4_1"
    let (Just input) = parseInputList raw
        xs = computeReports (M.empty, -1) input
        sleepiest = sleepiestGuard xs
        sleepMin = sleepiestMinute$ xs M.! sleepiest
    print sleepiest
    print sleepMin
    print $ xs M.! sleepiest
    print $ sleepiest * sleepMin
    where
        computeReports ::  (M.Map Natural MidnightHour, Natural) -> [(TS,Event)] -> M.Map Natural MidnightHour
        computeReports (acc, gid) [] = acc
        computeReports (acc, gid) ((_, GuardStart gid'):rest) = computeReports (acc, gid') rest
        computeReports (acc, gid) ((ts1, Asleep):(ts2, Awake):rest) = let
            MH mins = fromMaybe emptyMidnight $ M.lookup gid acc
            updated = foldr (\x m -> M.insertWith (+) x 1 m) mins [minute ts1.. (minute ts2) -1]
            in computeReports (M.insert gid (MH updated) acc, gid) rest

        sleepiestGuard :: M.Map Natural MidnightHour -> Natural
        sleepiestGuard = fst . maximumBy (comparing (mostTimeAsleep . snd)) . M.toList



newtype MidnightHour = MH (M.Map Natural Natural)
    deriving (Show)

emptyMidnight :: MidnightHour
emptyMidnight = MH M.empty

minutesAsleep :: MidnightHour -> Natural
minutesAsleep (MH m) = sum $ M.elems m

sleepiestMinute :: MidnightHour -> Natural
sleepiestMinute (MH m) = fst . maximumBy (comparing snd) $ M.toList m

mostTimeAsleep :: MidnightHour -> Natural
mostTimeAsleep (MH m) = snd . maximumBy (comparing snd) $ M.toList m

data GuardReport = GR {sleepTime :: MidnightHour, gID :: Natural}
    deriving (Show)

data Event
    = Asleep
    | Awake
    | GuardStart Natural
    deriving (Show)

data TS =
    TS {
        year :: Natural,
        month :: Natural,
        day :: Natural,
        hour :: Natural,
        minute :: Natural
    } deriving (Eq, Ord, Show) -- The order of the fields is important

parseInputList ::
    T.Text
    -> Maybe [(TS,Event)]
parseInputList = fmap (sortOn fst) . parseMaybe inputListParser

inputListParser :: Parsec () T.Text [(TS,Event)]
inputListParser =
    eventParser `sepEndBy` eol

eventParser :: Parsec () T.Text (TS, Event)
eventParser = do
    _ <- char '['
    ts <- tsParser
    _ <- char ']'
    _ <- char ' '
    action <- actionParser
    pure (ts, action)

tsParser :: Parsec () T.Text TS
tsParser = do
    y <- natParser
    _ <- char '-'
    m <- natParser
    _ <- char '-'
    d <- natParser
    _ <- char ' '
    h <- natParser
    _ <- char ':'
    n <- natParser
    pure $ TS y m d h n

actionParser :: Parsec () T.Text Event
actionParser = wakeParser <|> sleepParser <|> startParser
    where
        wakeParser = string "wakes up" *> pure Awake
        sleepParser = string "falls asleep" *> pure Asleep
        startParser = do
            _ <- string "Guard #"
            n <- natParser
            _ <- " begins shift"
            pure $ GuardStart n

natParser :: Parsec () T.Text Natural
natParser = read <$> many digitChar
