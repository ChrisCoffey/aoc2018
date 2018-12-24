module Problems.Thirteen (
    part1,
    part2
    ) where

import Data.Monoid ((<>))
import Data.List (sortBy, groupBy)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Ord (comparing)

import Debug.Trace

data TrackSegment
    = Line {start::Point, end::Point}
    | LTurn {a:: Point, b:: Point, c::Point, d::Point}
    | RTurn {a:: Point, b:: Point, c::Point, d::Point}
    | Intersection {left :: Point, right :: Point, up :: Point, down ::Point}
    deriving (Show, Eq, Ord)

newtype Point = Point (Int, Int)
    deriving (Eq, Show)
instance Ord Point where
   (Point (x,y)) <= (Point (x',y')) = (y,x) <= (y', x')

data Direction = U | D | L | R
    deriving (Eq, Ord, Show)
data NextTurn = LFT | RGT | ST
    deriving (Eq, Ord, Show)
data Cart = Cart {direction :: Direction, location :: Point, turning :: NextTurn}
    deriving (Eq, Ord, Show)

data TrackState
    = TS {
        carts :: [Cart],
        track :: M.Map Point TrackSegment
        }
    deriving (Show)

tick ::
    TrackState
    -> Cart
tick (TS carts track) = go [] carts
    where
        go (cart:rest) acc = let
            ct = cartTick cart
            in  if ( collisions (ct:rest)) || ( collisions (ct:acc))
                then ct
                else go rest (ct:acc)
        go [] acc =
            go (sortBy (comparing location) acc) []

        cartTick (Cart dir l turn) =
            case track M.! l of
                (Intersection l r u d) ->
                    case (dir, turn) of
                        (U, LFT) -> Cart L l ST
                        (U, RGT) -> Cart R r LFT
                        (U, ST) -> Cart U u RGT

                        (D, LFT) -> Cart R r ST
                        (D, RGT) -> Cart L l LFT
                        (D, ST) -> Cart D d RGT

                        (L, LFT) -> Cart D d ST
                        (L, RGT) -> Cart U u LFT
                        (L, ST) -> Cart L l RGT

                        (R, LFT) -> Cart U u ST
                        (R, RGT) -> Cart D d LFT
                        (R, ST) -> Cart R r RGT
                (LTurn l r u d) ->
                    case dir of
                        U -> Cart L l turn
                        D -> Cart R r turn
                        L -> Cart U u turn
                        R -> Cart D d turn
                (RTurn l r u d) ->
                    case dir of
                        U -> Cart R r turn
                        D -> Cart L l turn
                        L -> Cart D d turn
                        R -> Cart U u turn
                (Line a b) ->
                    case dir of
                        U -> Cart dir b turn
                        D -> Cart dir a turn
                        L -> Cart dir a turn
                        R -> Cart dir b turn

collisions ::
    [Cart]
    -> Bool
collisions carts =
    not . null . filter ((>= 2) . length) . groupBy (\l r -> location l == location r) $ sortBy (comparing location) carts

part1 :: IO ()
part1 = do
    input <- parseInput "data/13.test"
    let res = tick input
    print res

part2 :: IO ()
part2 = undefined

--
--
--

parseInput :: FilePath -> IO TrackState
parseInput fp = do
    lsx <- T.lines <$> TIO.readFile fp
    let coords = makeCoords lsx
        carts = makeCarts coords
        segments = makeSegments coords
    pure $ TS {carts = carts, track = M.fromList segments}

toSegment ::
    Point
    -> Char
    -> Maybe TrackSegment
toSegment (Point (x,y)) '-' = Just $ Line (Point (x-1, y)) (Point (x+1, y))
toSegment (Point (x,y)) '|' = Just $ Line (Point (x, y-1)) (Point (x, y+1))
toSegment (Point (x,y)) '+' = Just $ Intersection {
    left = (Point (x-1, y)),
    right = (Point (x+1, y)),
    up = (Point (x, y+1)),
    down = (Point (x, y-1))
    }
toSegment (Point (x,y)) '>' = Just $ Line (Point (x-1, y)) (Point (x+1, y))
toSegment (Point (x,y)) '<' = Just $ Line (Point (x-1, y)) (Point (x+1, y))
toSegment (Point (x,y)) '^' = Just $ Line (Point (x, y-1)) (Point (x, y+1))
toSegment (Point (x,y)) 'v' = Just $ Line (Point (x, y-1)) (Point (x, y+1))
toSegment point ' ' = Nothing
toSegment (Point (x,y)) '/' = Just $ RTurn (Point (x-1,y)) (Point (x+1,y))
                                           (Point (x,y+1)) (Point (x,y-1))
toSegment (Point (x,y)) '\\' = Just $ LTurn (Point (x-1,y)) (Point (x+1,y))
                                            (Point (x,y+1)) (Point (x,y-1))

makeCoords ::
    [T.Text]
    -> [(Point, Char)]
makeCoords lsx = [
    (Point (x,y), character) |
        (y, ln) <- zip [0, -1..] lsx,
        (x, character) <- zip [0..] (T.unpack ln)
    ]

makeSegments ::
    [(Point, Char)]
    -> [(Point, TrackSegment)]
makeSegments pts =
    [(p, fromJust $ toSegment p c) | (p,c) <- pts, isJust (toSegment p c)]

makeCarts ::
    [(Point, Char)]
    -> [Cart]
makeCarts pts = [ toC p c | (p,c) <- pts, c `elem` ['>', '<', '^', 'v']]
    where
        toC p '>' = Cart R p LFT
        toC p '<' = Cart L p LFT
        toC p '^' = Cart U p LFT
        toC p 'v' = Cart D p LFT
