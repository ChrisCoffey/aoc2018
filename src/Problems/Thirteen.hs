module Problems.Thirteen (
    part1,
    part2
    ) where

import Control.Concurrent (threadDelay)
import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Data.List (sortBy, groupBy)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Ord (comparing)

import Debug.Trace

data TrackSegment
    = Horizontal
    | Vertical
    | LTurn
    | RTurn
    | Intersection
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
    -> IO Cart
tick (TS carts track) = go [] carts
    where
        go (cart:rest) acc = let
            ct = cartTick cart
            in  if ( collisions (ct:rest)) || ( collisions (ct:acc))
                then pure ct
                else go rest (ct:acc)
        go [] acc = do
            -- printFrame (TS acc track)
            go oc []
            where
                oc = reverse $ sortBy (comparing location) acc

        cartTick c@(Cart dir l@(Point (x,y)) turn) =
            case track M.! l of
                Intersection ->
                    case (dir, turn) of
                        (U, LFT) -> Cart L (Point (x-1,y)) ST
                        (U, ST) -> Cart U (Point (x,y+1)) RGT
                        (U, RGT) -> Cart R (Point (x+1, y)) LFT

                        (D, LFT) -> Cart R (Point (x+1,y)) ST
                        (D, ST) -> Cart D (Point (x,y-1)) RGT
                        (D, RGT) -> Cart L (Point (x-1,y)) LFT

                        (L, LFT) -> Cart D (Point (x,y-1)) ST
                        (L, ST) -> Cart L (Point (x-1,y)) RGT
                        (L, RGT) -> Cart U (Point (x,y+1)) LFT

                        (R, LFT) -> Cart U (Point (x,y+1)) ST
                        (R, ST) -> Cart R (Point (x+1,y)) RGT
                        (R, RGT) -> Cart D (Point (x,y-1)) LFT
                LTurn ->
                    case dir of
                        U -> Cart L (Point (x-1, y)) turn
                        D -> Cart R (Point (x+1,y)) turn
                        L -> Cart U (Point (x,y+1)) turn
                        R -> Cart D (Point (x,y-1)) turn
                RTurn ->
                    case dir of
                        U -> Cart R (Point (x+1, y)) turn
                        D -> Cart L (Point (x-1,y)) turn
                        L -> Cart D (Point (x,y-1)) turn
                        R -> Cart U (Point (x,y+1)) turn
                Horizontal ->
                    case dir of
                        L -> Cart dir (Point (x-1, y)) turn
                        R -> Cart dir (Point (x+1, y)) turn
                Vertical ->
                    case dir of
                        U -> Cart dir (Point (x,y+1)) turn
                        D -> Cart dir (Point (x,y-1)) turn

collisions ::
    [Cart]
    -> Bool
collisions carts =
    any ((>= 2) . length) . groupBy (\l r -> location l == location r) $
        sortBy (comparing location) carts

part1 :: IO ()
part1 = do
    input <- parseInput "data/13.data"
    res <- tick input
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
    Char
    -> Maybe TrackSegment
toSegment '-' = Just Horizontal
toSegment '>' = Just Horizontal
toSegment '<' = Just Horizontal
toSegment '|' = Just Vertical
toSegment '^' = Just Vertical
toSegment 'v' = Just Vertical
toSegment '+' = Just Intersection
toSegment '/' = Just RTurn
toSegment '\\' = Just LTurn
toSegment ' ' = Nothing

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
    [(p, fromJust $ toSegment c) | (p,c) <- pts, isJust (toSegment c)]

makeCarts ::
    [(Point, Char)]
    -> [Cart]
makeCarts pts = [ toC p c | (p,c) <- pts, c `elem` ['>', '<', '^', 'v']]
    where
        toC p '>' = Cart R p LFT
        toC p '<' = Cart L p LFT
        toC p '^' = Cart U p LFT
        toC p 'v' = Cart D p LFT

printFrame ::
    TrackState
    -> IO ()
printFrame (TS carts ts ) = do
    threadDelay 250000
    putStr "\ESC[2J"
    mapM_ putStrLn points
    where
        points :: [String]
        points = lines $ coordChar <$> [ (x,y) | y <- [0,-1..(-150)], x <- [0..150] ]
        cartMap = M.fromList $ (\k -> (location k, k)) <$> carts
        coordChar px = fromJust $
            nl px <|>
            (cartToChar <$> M.lookup (Point px) cartMap ) <|>
            (segmentToChar <$> M.lookup (Point px) ts ) <|>
            pure ' '

        nl (150, _) = Just '\n'
        nl _ = Nothing

        segmentToChar Horizontal = '-'
        segmentToChar Vertical = '|'
        segmentToChar LTurn = '\\'
        segmentToChar RTurn = '/'
        segmentToChar Intersection = '+'

        cartToChar (Cart R _ _) = '>'
        cartToChar (Cart L _ _) = '<'
        cartToChar (Cart U _ _) = '^'
        cartToChar (Cart D _ _) = 'v'
