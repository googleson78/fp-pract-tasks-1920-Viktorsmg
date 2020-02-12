module Stuff
    ( group
    , sortBy
    , groupBy
    , sortOn
    , groupOn
    , classifyOn
    , (&&&)
    , on
    , tupify) where


insertGroup :: (a -> a -> Bool) -> a -> [[a]] -> [[a]]
insertGroup _ x [] = [[x]]
insertGroup _ x [[]] = [[x]]
insertGroup _ x ([]:yss) = [x]:yss -- <- this?
insertGroup eq x ((y:ys):yss)
    | x `eq` y = (x:y:ys):yss
    | otherwise = [x]:(y:ys):yss

insertGroupGlobal :: (a -> a -> Bool) -> a -> [[a]] -> [[a]]
insertGroupGlobal _ x [] = [[x]]
insertGroupGlobal _ x [[]] = [[x]]
insertGroupGlobal eq x ([]:yss) = insertGroupGlobal eq x yss -- <- this?
insertGroupGlobal eq x ((y:ys):yss)
    | x `eq` y = (x:y:ys):yss
    | otherwise = (y:ys):insertGroupGlobal eq x yss

superGrouper :: ((a -> a -> Bool) -> a -> [[a]] -> [[a]]) -> (a -> a -> Bool) -> [a] -> [[a]]
superGrouper _ _ [] = []
superGrouper inserter eq (x:xs) = inserter eq x $ superGrouper inserter eq xs

group :: Eq a => [a] -> [[a]]
group = superGrouper insertGroup (==)

-- Not mandatory, delete if you don't want this.
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ x [] = [x]
insertBy comp x (sx:sxs)
    | comp x sx == LT = x:sx:sxs
    | otherwise = sx:insertBy comp x sxs

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy comp (x:xs) = insertBy comp x $ sortBy comp xs
 
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy = superGrouper insertGroup

-- Example use: on compare fst (5, dasfas) (8, fjda) -> LT
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on dual mapper x y = dual (mapper x) $ mapper y

-- Example use: (&&&) (+1) (+2) 0 -> (1, 2)
(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f1 f2 x = (f1 x, f2 x)

tupify :: a -> b -> (a, b)
tupify x y = (x, y)

-- If sortOn wasn't throwing a wrench in my plans, I'd have turned the 3 functions below into a single superSomething that does the memory list and whatever

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn mapper xs = map snd sortedTups where
    sortedTups = sortBy (on compare fst) memoryList
    memoryList = zipWith tupify (map mapper xs) xs

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn mapper xs = map (map snd) groupedTups where
    groupedTups = groupBy (on (==) fst) memoryList
    memoryList = zipWith tupify (map mapper xs) xs

classifyOn :: Eq b => (a -> b) -> [a] -> [[a]]
classifyOn mapper xs = map (map snd) classedTups where
    classedTups = superGrouper insertGroupGlobal (on (==) fst) memoryList
    memoryList = zipWith tupify (map mapper xs) xs 


