module Problems.Nine (
    part1,
    part2
) where

import Prelude hiding (length, splitAt)
import Data.Foldable (toList)
import Data.Sequence

data PlayerState = PS {name:: Int, score :: Int}
    deriving (Show)

newtype Stone = S Int
    deriving (Show, Eq, Ord, Num)

newtype Ring = Ring (Seq Stone)
    deriving Show

-- Mechanics of actually adding a stone to the ring. The 'Current' stone is always the head of the list
placeStone ::
    Stone
    -> Ring
    -> Ring
placeStone s (Ring xs) =
    case toList xs of
        [] -> Ring $ singleton s
        [x] -> Ring $ s <| xs
        [x,y] -> Ring $ fromList [s,x,y]
        _ -> let
            current :< xs' = viewl xs
            x :< ys = viewl xs'
            y :< rest = viewl ys
            in Ring $ ((s <| (y <| rest))  |> current) |> x

turn ::
    PlayerState
    -> Stone
    -> Ring
    -> (PlayerState, Ring)
turn ps s@(S n) ring
    | n `mod` 23 == 0 && n > 0= let
        (ring', S x) = removeNthCC 7 ring
        in (ps {score = score ps + n + x}, ring')
    | otherwise = (ps, placeStone s ring)

removeNthCC ::
    Int
    -> Ring
    -> (Ring, Stone)
removeNthCC n (Ring xs) = let
    (as,zs) = splitAt (length xs - n) xs
    (s :< bs) = viewl zs
    in (Ring (bs<>as), s)

playGame ::
    (Seq PlayerState, [Stone], Ring)
    -> Seq PlayerState
playGame (states, [],_) = states
playGame (states, s:stones, ring) =
    case viewl states of
        (ps :< rest) -> let
            (nextState, ring') = turn ps s ring
            in playGame (rest |> nextState, stones, ring')


winner ::
    [PlayerState]
    -> PlayerState
winner [ps] = ps
winner (ps:a:rest)
    | score ps > score a =  winner (ps:rest)
    | otherwise = winner (a:rest)

part1 :: IO ()
part1 = do
    let players = fromList [PS pn 0 | pn <- [1..463]]
        stones = S <$> [0..71787]
    print . winner . toList $ playGame (players, stones, Ring empty)

part2 :: IO ()
part2 = do
    let players = fromList [PS pn 0 | pn <- [1..463]]
        stones = S <$> [0..7178700]
    print . winner . toList $ playGame (players, stones, Ring empty)
