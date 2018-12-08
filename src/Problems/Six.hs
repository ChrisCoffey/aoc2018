module Problems.Six (
    part1,
    part2
) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sort, sortOn, maximumBy, minimumBy, group)
import Data.Ord (comparing)
import Data.Maybe (catMaybes)
import Control.Arrow ((&&&))

data Point = Point {
    x :: Int,
    y :: Int
    }
    deriving (Eq, Show, Ord)

toPoint :: String -> Point
toPoint str = let
    x = read $ takeWhile (/= ',') str
    y = read . tail $ dropWhile (/= ' ') str
    in Point {x = x, y = y}

part2 :: IO ()
part2 = do
    pts <- fmap toPoint . lines  <$> readFile "data/6_1"
    let minX = (\a -> a- 5) . x $ minimumBy (comparing x) pts
        minY = (\a -> a- 5) . y $ minimumBy (comparing y) pts
        maxX = (+ 5) . x $ maximumBy (comparing x) pts
        maxY = (+ 5) . y $ maximumBy (comparing y) pts
        searchSpace = [Point a b | a <- [minX..maxX], b <- [minY..maxY]]
        distances = totalDistance pts <$> searchSpace
        relevantPoints = filter (< 10000) distances
    print $ length relevantPoints
    where
        totalDistance pts p = sum $ manhattanDistance p <$> pts


part1 :: IO ()
part1 = do
    pts <- fmap toPoint . lines  <$> readFile "data/6_1"
    let minX = (\a -> a- 5) . x $ minimumBy (comparing x) pts
        minY = (\a -> a- 5) . y $ minimumBy (comparing y) pts
        maxX = (+ 5) . x $ maximumBy (comparing x) pts
        maxY = (+ 5) . y $ maximumBy (comparing y) pts
        searchSpace = [Point a b | a <- [minX..maxX], b <- [minY..maxY]]
        areas = catMaybes $ nearestNode pts <$> searchSpace
        infiniteAreas = S.fromList . xEdge minX . xEdge maxX . yEdge minY $ yEdge maxY areas
        applicableAreas = filter (not . (`S.member` infiniteAreas) )areas
    print $ fmap length . take 1 . reverse . sortOn length . group $ sort applicableAreas
    where
    xEdge xVal points = filter ((== xVal) . x) points
    yEdge yVal points = filter ((== yVal) . y) points



-- Attempt to avoid processing everything. Definitely not worth it, and buggy. Brute force solution above

-- Compute a convex hull, then determine the nearest Node for all points inside the hull. Return the node
-- that is both not an edge and appears the most in the results. If two nodes are the same distance, return `dupe`
part1Hull :: IO ()
part1Hull = do
    pts <- fmap toPoint . lines  <$> readFile "data/6_1"
    let hull = convexHull pts
        -- Compute a bounding box for the search space
        minX = x $ minimumBy (comparing x) pts
        minY = y $ minimumBy (comparing y) pts
        maxX = x $ maximumBy (comparing x) pts
        maxY = y $ maximumBy (comparing y) pts
        searchSpace = [Point a b | a <- [minX..maxX], b <- [minY..maxY], inHull (Point a b) hull]
        distances = catMaybes $ nearestNode pts <$> searchSpace
        nonHullPoints = filter (not . flip elem (fst <$> unHull hull)) distances
        largestArea = length . last . sortOn length . group $ sort nonHullPoints
    print largestArea


zipUsing ::
    (a -> b)
    -> [a]
    -> [(b,a)]
zipUsing f xs = zip (f <$> xs) xs

nearestNode ::
    [Point]
    -> Point
    -> Maybe Point
nearestNode nonHullPoints point = let
    distances = [(manhattanDistance p point, p) | p <- nonHullPoints]
    dist = minimum distances
    in if length (filter ((== (fst dist)) . fst ) distances) == 1
       then Just $ snd dist
       else Nothing

manhattanDistance ::
    Point
    -> Point
    -> Int
manhattanDistance (Point x y) (Point x' y') =
    abs (x - x') + abs (y - y')

newtype Hull = Hull {unHull :: [(Point, Point)] }
    deriving (Eq, Ord, Show)

convexHull ::
    [Point]
    -> Hull
convexHull points =
    Hull $ (last hull, head hull): zip hull (tail hull)
    where
    hull = upper <> lower
    upper = hullChain $ reverse sorted
    lower = hullChain sorted
    sorted = sort points

    minusP (Point x y) (Point x' y') = Point (x-x') (y-y')
    crossP (Point x y) (Point x' y') = (x*y') - (x'*y)
    isClockwiseRotation o a b = (a `minusP` o) `crossP` (b `minusP` o) <= 0
    hullChain xs = go [] xs
        where
        go :: [Point] -> [Point] -> [Point]
        go acc [] = reverse $ tail acc
        go acc@(x:x':chain) (a:as) =
            if isClockwiseRotation x' x a
            then go (x':chain) (a:as) -- turned clockwise, wrong direction so backtrack & drop the head of the chain
            else go (a:acc) as
        go acc (a:as) = go (a:acc) as


-- Taken from rosettacode

data Line = Sloped {lineSlope, lineYIntercept :: Int} |
            Vert {lineXIntercept :: Int}

intersects :: Point -> Line -> Bool
intersects (Point px _)  (Vert xint)  = px <= xint
intersects (Point px py) (Sloped m b)
    | m < 0     = py <= m * px + b
    | otherwise = py >= m * px + b

onLine :: Point -> Line -> Bool
onLine (Point px _)  (Vert xint)  = px == xint
onLine (Point px py) (Sloped m b) = py == m * px + b

carrier :: (Point, Point) -> Line
carrier ((Point ax ay), (Point bx by)) | ax == bx  = Vert ax
                             | otherwise = Sloped slope yint
  where slope = (ay - by) `div` (ax - bx)
        yint = ay - slope * ax

between :: Ord a => a -> a -> a -> Bool
between x a b | a > b     = b <= x && x <= a
              | otherwise = a <= x && x <= b

inHull :: Point -> Hull -> Bool
inHull p@(Point px py) = f 0 . unHull
  where f n []                             = odd n
        f n (side : sides) | far           = f n       sides
                           | onSegment     = True
                           | rayIntersects = f (n + 1) sides
                           | otherwise     = f n       sides
          where far = not $ between py ay by
                onSegment | ay == by  = between px ax bx
                          | otherwise = p `onLine` line
                rayIntersects =
                    intersects p line &&
                    (py /= ay || by < py) &&
                    (py /= by || ay < py)
                ((Point ax ay), (Point bx by)) = side
                line = carrier side
