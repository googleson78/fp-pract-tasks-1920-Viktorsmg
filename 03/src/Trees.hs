{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin

module Trees where

import Prelude
import Data.Monoid (Sum(..), All(..), Any(..), First(..))
import Data.Maybe (isJust)

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving Show

data TreeDir = TDLeft | TDRight
  deriving (Show, Eq)

compareTreeVal :: Ord a => TreeDir -> a -> a -> Bool
compareTreeVal TDRight val1 val2 = val1 > val2
compareTreeVal TDLeft val1 val2 = val1 <= val2

whereToGo :: Ord a => a -> a -> TreeDir
whereToGo val1 val2 = if compareTreeVal TDRight val1 val2 then TDRight else TDLeft

-- Get subtree via direction
getSubtreeDir :: TreeDir -> Tree a -> Tree a
getSubtreeDir _ Empty = Empty
getSubtreeDir TDLeft (Node _ l _) = l
getSubtreeDir TDRight (Node _ _ r) = r

-- Get subtree by comparing a value with the tree's node value
getSubtreeComp :: Ord a => a -> Tree a -> Tree a
getSubtreeComp _ Empty = Empty
getSubtreeComp val tr@(Node cmpval _ _) = getSubtreeDir (whereToGo val cmpval) tr

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) Empty Empty = True
  (==) Empty _ = False
  (==) _ Empty = False
  (==) (Node val1 l1 r1) (Node val2 l2 r2) = (val1 == val2) && (l1 == l2) && (r1 == r2)

insertOrdered :: Ord a => a -> Tree a -> Tree a
insertOrdered val Empty = Node val Empty Empty
insertOrdered val (Node compVal l r) = if whereToGo val compVal == TDLeft then Node compVal (insertOrdered val l) r else Node compVal l (insertOrdered val r)

listToBST :: Ord a => [a] -> Tree a
listToBST = foldl (flip insertOrdered) Empty

isBSTH :: Ord a => a -> TreeDir -> Tree a -> Bool
isBSTH _ _ Empty = True
isBSTH val dir (Node compVal l r) = compareTreeVal dir compVal val
  && isBSTH val dir l && isBSTH compVal TDLeft l
  && isBSTH val dir r && isBSTH compVal TDRight r
-- 

isBST :: Ord a => Tree a -> Bool
isBST Empty = True
isBST (Node val l r) = isBSTH val TDLeft l && isBSTH val TDRight r

-- idea for implementing isBST - delete if you don't want it
--data BotTop a = Bot | Val a | Top
--  deriving (Show, Eq, Ord)
--
--between :: Ord a => BotTop a -> BotTop a -> Tree a -> Bool
--between = undefined

findBST :: Ord a => a -> Tree a -> Bool
findBST _ Empty = False
findBST val tr@(Node trval _ _) = val==trval || findBST val (getSubtreeComp val tr)


-- The actual all-powerful tree function.
-- Its sub-function takes <Folded left subtree> <Direct value> <Folded right subtree>
customFoldTree :: (b -> a -> b -> b) -> b -> Tree a -> b
customFoldTree _ nv Empty = nv
customFoldTree f nv (Node val l r) = f (customFoldTree f nv l) val (customFoldTree f nv r)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = customFoldTree (\l c r -> Node (f c) l r) Empty

foldTree :: Monoid a => Tree a -> a
foldTree = customFoldTree (\ l c r -> (l <> c) <> r) mempty

foldMapTree :: Monoid b => (a -> b) -> Tree a -> b
foldMapTree f = customFoldTree (\ l c r -> (l <> f c) <> r) mempty

sumTree :: Num a => Tree a -> a
sumTree = customFoldTree (\ l c r -> l + c + r) 0

allTree :: (a -> Bool) -> Tree a -> Bool
allTree f tr = customFoldTree (\ l c r -> l && c && r) True (mapTree f tr)

anyTree :: (a -> Bool) -> Tree a -> Bool
anyTree f tr = customFoldTree (\ l c r -> l || c || r) False (mapTree f tr)

treeToList :: Tree a -> [a]
treeToList = customFoldTree (\ l c r -> l++[c]++r) []

elemTree :: Eq a => a -> Tree a -> Bool
elemTree which = anyTree (==which)


onMaybe :: (a -> Bool) -> a -> Maybe a
onMaybe p x = if p x then Just x else Nothing

firstJust :: Maybe a -> Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ _ = Just x
firstJust _ (Just y) _ = Just y
firstJust _ _ (Just z) = Just z
firstJust _ _ _ = Nothing

findPred :: (a -> Bool) -> Tree a -> Maybe a
findPred p = customFoldTree (\ l c r -> firstJust l (onMaybe p c) r) Nothing

onList :: (a -> Bool) -> a -> [a]
onList p x = [x | p x]

findAll :: (a -> Bool) -> Tree a -> [a]
findAll p = customFoldTree (\ l c r -> l ++ onList p c ++ r) []

ifJust :: Maybe a -> (a -> Maybe b) -> Maybe b
ifJust Nothing _ = Nothing
ifJust (Just x) f = f x

maybeNotEmpty :: Maybe a -> Bool
maybeNotEmpty Nothing = False
maybeNotEmpty _ = True

validateTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
validateTree p = customFoldTree (\l c r -> case (l, p c, r) of (Just trl, Just val, Just trr) -> Just (Node val trl trr); _ -> Nothing) (Just Empty)

-- :<
-- And I just made my own direction type
data Direction
  = L -- go left
  | R -- go right
  deriving (Show, Eq)

-- This is probably doable with the custom fold, but I won't do it.
fetch :: [Direction] -> Tree a -> Maybe a
fetch [] (Node val _ _) = Just val
fetch _ Empty = Nothing
fetch (L:dirs) (Node _ l _) = fetch dirs l 
fetch (R:dirs) (Node _ _ r) = fetch dirs r

addDirection :: Direction -> (a, [Direction]) -> (a, [Direction])
addDirection dir (val, dirs) = (val, dir:dirs)

paths :: Tree a -> [(a, [Direction])]
paths = customFoldTree (\ lPaths c rPaths -> map (addDirection L) lPaths ++ [(c, [])] ++ map (addDirection R) rPaths) []