module Problems.Sixteen (
    part1,
    part2
    ) where

import Data.Bits ((.&.), (.|.))
import Data.Maybe (fromJust)
import Data.List (sortBy, groupBy, sort, group, intersect)
import Data.Foldable (foldl')
import qualified Data.Map as M
import Data.Ord (comparing)
import Text.Megaparsec
import Text.Megaparsec.Char (eol, digitChar, spaceChar, char, string)

import Debug.Trace

--
-- Input
--
data Input = Input {p1 :: [MysteryOp], p2 :: [Registers]}
    deriving (Show)

data MysteryOp = Mystery {
    before :: Registers,
    inst :: Registers,
    after :: Registers}
    deriving ( Show)

sortByCode ::
    MysteryOp
    -> MysteryOp
    -> Ordering
sortByCode = comparing byCode
    where
        byCode Mystery {inst} = rCode inst

eqByCode ::
    MysteryOp
    -> MysteryOp
    -> Bool
eqByCode l r = byCode l == byCode r
    where
        byCode Mystery {inst} = rCode inst
--
-- Interesting bits
--

newtype OpCode = Op {op :: Int}
    deriving (Eq, Show, Ord)

newtype Registers = R (Int, Int, Int, Int)
    deriving (Eq, Show, Ord)


data OpClass
    = AddR
    | AddI
    | MulR
    | MulI
    | BanR
    | BanI
    | BorR
    | BorI
    | SetR
    | SetI
    | GtiR
    | GtrI
    | GtrR
    | EqiR
    | EqrI
    | EqrR
    deriving (Eq, Ord, Show, Bounded, Enum)

runOpClass ::
    OpClass
    -> Registers -- ^ before
    -> Registers -- ^ inst
    -> Registers
runOpClass AddR = rRegisterBinOp (+)
runOpClass AddI = rImmediateBinOp (+)
runOpClass MulR = rRegisterBinOp (*)
runOpClass MulI = rImmediateBinOp (*)
runOpClass BanR = rRegisterBinOp (.&.)
runOpClass BanI = rImmediateBinOp (.&.)
runOpClass BorR = rRegisterBinOp (.|.)
runOpClass BorI = rImmediateBinOp (.|.)
runOpClass SetR = rRegisterBinOp const
runOpClass opClass = \ reg@(R r) (R (code, a, b, c)) ->
    case opClass of
        SetI  -> setOutput reg c a
        GtiR  -> setOutput reg c $ if a > rIdx b r then 1 else 0
        GtrI  -> setOutput reg c $ if rIdx a r > b then 1 else 0
        GtrR  -> setOutput reg c $ if rIdx a r > rIdx b r then 1 else 0
        EqiR  -> setOutput reg c $ if a == rIdx b r then 1 else 0
        EqrI  -> setOutput reg c $ if rIdx a r == b then 1 else 0
        EqrR  -> setOutput reg c $ if rIdx a r == rIdx b r then 1 else 0
        _ -> error "Odd opcode error"

rRegisterBinOp ::
    (Int -> Int -> Int)
    -> Registers -- ^ before
    -> Registers
    -> Registers
rRegisterBinOp op reg@(R r) (R (code, a, b, c)) = setOutput reg c $ rIdx a r  `op` rIdx b r

rImmediateBinOp ::
    (Int -> Int -> Int)
    -> Registers -- ^ before
    -> Registers
    -> Registers
rImmediateBinOp op reg@(R r) (R (code, a, b, c)) = setOutput reg c $ rIdx a r `op` b

rIdx 0 (x, _, _, _) = x
rIdx 1 (_, x, _, _) = x
rIdx 2 (_, _, x, _) = x
rIdx 3 (_, _, _, x) = x
rIdx _ _ = error "Register Index out of bounds"

setOutput ::
    Registers
    -> Int
    -> Int
    -> Registers
setOutput (R (i,a,b,c)) 0 n = R (n,a,b,c)
setOutput (R (i,a,b,c)) 1 n = R (i,n,b,c)
setOutput (R (i,a,b,c)) 2 n = R (i,a,n,c)
setOutput (R (i,a,b,c)) 3 n = R (i,a,b,n)


findOpcodeCandidates ::
    [MysteryOp]
    -> (Int, [OpClass])
findOpcodeCandidates [] = error "Could not determine OpClass"
findOpcodeCandidates mysteries = let
    a = filter ((== mCount) . length). group . sort . concat $ matches <$> mysteries
    in (code,) . concatMap (take 1) $ a
    where
        mCount = length mysteries
        code = rCode . inst $ head mysteries

findOpcodes ::
    M.Map Int [OpClass]
    -> M.Map Int OpClass
    -> M.Map Int OpClass
findOpcodes candidates found
    | M.size candidates == M.size found = found
    | otherwise = findOpcodes candidates (M.union solved found)
    where
        candidates' = filter (`notElem` foundCodes) <$> candidates
        foundCodes = M.elems found
        solved =  fmap head $ M.filter ((== 1) . length) candidates'

rCode (R (code, _, _, _)) = code

matches Mystery {before, inst, after} = [op | op <- [minBound..maxBound]
                                                  , runOpClass op before inst == after ]

part1 :: IO ()
part1 = do
    raw <- readFile "data/16.data"
    let input = fromJust $ parseInput raw
    print . length $ matches (Mystery (R (3,2,1,1)) (R (9, 2, 1, 2)) (R (3,2,2,1)))
    print $ length . filter (>=3) $ length . matches <$> p1 input


part2 :: IO ()
part2 = do
    raw <- readFile "data/16.data"
    let input = fromJust $ parseInput raw
        codeGroups = groupBy eqByCode . sortBy sortByCode $ p1 input
        codes = findOpcodeCandidates <$> codeGroups
        codeIdx = M.fromList codes
        opClasses = findOpcodes codeIdx M.empty
        res = foldl' (applyOp opClasses) (R (0,0,0,0)) $ p2 input
    print res
    where
        applyOp ops regs inst = runOpClass (ops M.! rCode inst) regs inst


--
-- Cruft
--

parseInput :: String -> Maybe Input
parseInput = parseMaybe inputParser

inputParser :: Parsec () String Input
inputParser = do
    p1 <- sepEndBy mysteryOpParser (many eol)
    _ <- many eol
    p2 <- sepEndBy parseInstruction eol
    pure $ Input p1 p2

mysteryOpParser :: Parsec () String MysteryOp
mysteryOpParser = do
    _ <- string "Before: "
    before <- parseCsv
    _ <- eol
    inst <- parseInstruction
    _ <- eol
    _ <- string "After:  "
    after <- parseCsv
    pure $ Mystery {before = before, after = after, inst = inst}
    where

parseCsv = do
    _ <- char '['
    code <- intParser
    _ <- string ", "
    a <- intParser
    _ <- string ", "
    b <- intParser
    _ <- string ", "
    c <- intParser
    _ <- char ']'
    pure $ R (code, a, b, c)

parseInstruction :: Parsec () String Registers
parseInstruction = do
    code <- read <$> many digitChar
    _ <- spaceChar
    a <- intParser
    _ <- spaceChar
    b <- intParser
    _ <- spaceChar
    c <- intParser
    pure $ R (code, a, b, c)

intParser :: Parsec () String Int
intParser = read <$> many digitChar
