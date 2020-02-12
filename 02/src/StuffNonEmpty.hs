module StuffNonEmpty
    ( NonEmpty(..)
    , mapNonEmpty
    , groupNonEmpty
    , groupByNonEmpty
    , groupOnNonEmpty
    , classifyOnNonEmpty
    ) where

import Stuff (sortOn, sortBy, on, (&&&), tupify)

data NonEmpty a = a :| [a]
    deriving (Show, Eq, Ord)
infixr 4 :|

insertGroupNE :: (a -> a -> Bool) -> a -> [NonEmpty a] -> [NonEmpty a]
insertGroupNE _ x [] = [x:|[]]
insertGroupNE eq x ((y:|ys):yss)
    | x `eq` y = (x:|y:ys):yss
    | otherwise = (x:|[]):(y:|ys):yss

insertGroupGlobalNE :: (a -> a -> Bool) -> a -> [NonEmpty a] -> [NonEmpty a]
insertGroupGlobalNE _ x [] = [x:|[]]
insertGroupGlobalNE eq x ((y:|ys):yss)
    | x `eq` y = (x:|y:ys):yss
    | otherwise = (y:|ys):insertGroupGlobalNE eq x yss

superGrouperNE :: ((a -> a -> Bool) -> a -> [NonEmpty a] -> [NonEmpty a]) -> (a -> a -> Bool) -> [a] -> [NonEmpty a]
superGrouperNE _ _ [] = []
superGrouperNE _ _ [x] = [x:|[]]
superGrouperNE inserter eq (x:xs) = inserter eq x $ superGrouperNE inserter eq xs

groupNonEmpty :: Eq a => [a] -> [NonEmpty a]
groupNonEmpty = superGrouperNE insertGroupNE (==)

mapNE :: (a -> b) -> NonEmpty a -> NonEmpty b
mapNE f (a:|xs) = (f a):|(map f xs)

mapNonEmpty :: (a -> b) -> NonEmpty a -> NonEmpty b
mapNonEmpty = mapNE

groupByNonEmpty :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupByNonEmpty = superGrouperNE insertGroupNE

groupOnNonEmpty :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
groupOnNonEmpty mapper xs = map (mapNE snd) groupedTups where
    groupedTups = groupByNonEmpty (on (==) fst) memoryList
    memoryList = zipWith tupify (map mapper xs) xs

classifyOnNonEmpty :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
classifyOnNonEmpty mapper xs = map (mapNE snd) classedTups where
    classedTups = superGrouperNE insertGroupGlobalNE (on (==) fst) memoryList
    memoryList = zipWith tupify (map mapper xs) xs 

-- "D:\\Users\\vikto_000\\Documents\\gh-repos\\fp-pract-tasks-1920-Viktorsmg\\02\\src\\StuffNonEmpty.hs"