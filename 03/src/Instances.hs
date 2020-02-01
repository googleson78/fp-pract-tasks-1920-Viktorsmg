{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin

module Instances where

import Prelude hiding (reverse)

import Data.Char (isSpace)
import Data.Function (on)

newtype Pointwise a b = Pointwise {getPointwise :: (a, b)}
  deriving (Show, Eq)

instance (Ord a, Ord b) => Ord (Pointwise a b) where
  (<=) :: Pointwise a b -> Pointwise a b -> Bool
  (<=) (Pointwise (a1, b1)) (Pointwise (a2, b2)) = (a1 <= a2) && (b1 <= b2)

newtype Lexicographic a b = Lexicographic {getLexicographic :: (a, b)}
  deriving (Show, Eq)

-- The default instance for tuples and lists
instance (Ord a, Ord b) => Ord (Lexicographic a b) where
  (<=) :: Lexicographic a b -> Lexicographic a b -> Bool
  (<=) (Lexicographic (a1, b1)) (Lexicographic (a2, b2)) = if a1 /= a2 then a1 <= a2 else b1 <= b2

newtype Fun a b = Fun {getFun :: a -> b}

instance (Semigroup b) => Semigroup (Fun a b) where
  (<>) :: Fun a b -> Fun a b -> Fun a b
  (<>) fun1 fun2 = Fun (\ x -> getFun fun1 x <> getFun fun2 x) 

instance (Monoid b) => Monoid (Fun a b) where
  mempty :: Fun a b
  mempty = Fun (const mempty)

newtype First a = First {getFirst :: Maybe a}
  deriving (Eq, Show)

instance Semigroup (First a) where
  (<>) :: First a -> First a -> First a
  (<>) (First (Just x)) _ = First (Just x)
  (<>) _ (First (Just y)) = First (Just y)
  (<>) _ _ = First Nothing

instance Monoid (First a) where
  mempty :: First a
  mempty = First Nothing

newtype Last a = Last {getLast :: Maybe a}
  deriving (Eq, Show)

instance Semigroup (Last a) where
  (<>) :: Last a -> Last a -> Last a
  (<>) _ (Last (Just y)) = Last (Just y)
  (<>) (Last (Just x)) _ = Last (Just x)
  (<>) _ _ = Last Nothing

instance Monoid (Last a) where
  mempty :: Last a
  mempty = Last Nothing

newtype Pair a b = Pair {getPair :: (a, b)}
  deriving (Show, Eq)

-- The default instance for tuples
instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (<>) :: Pair a b -> Pair a b -> Pair a b
  (<>) (Pair (x1, y1)) (Pair (x2, y2)) = Pair (x1 <> x2, y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty :: Pair a b
  mempty = Pair (mempty, mempty)

newtype Dual a = Dual {getDual :: a}
  deriving (Show, Eq)

instance Semigroup a => Semigroup (Dual a) where
  (<>) :: Dual a -> Dual a -> Dual a
  (<>) (Dual val1) (Dual val2) = Dual (val2 <> val1)

instance Monoid a => Monoid (Dual a) where
  mempty :: Dual a
  mempty = Dual mempty

reverse :: [a] -> [a]
reverse xs = getDual (foldMap (Dual . (:[])) xs)

data Flux a = Flux
  { sides :: Maybe (a, a)
  , changes :: Int
  }
  deriving (Show, Eq)

flux :: a -> Flux a
flux x = Flux (Just (x, x)) 0

instance (Eq a) => Semigroup (Flux a) where
  (<>) :: Flux a -> Flux a -> Flux a
  (<>) (Flux Nothing _) fl = fl
  (<>) fl (Flux Nothing _) = fl
  (<>) (Flux (Just (fst1, snd1)) cgs1) (Flux (Just (fst2, snd2)) cgs2) = Flux (Just (fst1, snd2)) (cgs1+cgs2+(if snd1 /= fst2 then 1 else 0))

instance (Eq a) => Monoid (Flux a) where
  mempty :: Flux a
  mempty = Flux Nothing 0

-- "D:\\Users\\vikto_000\\Documents\\gh-repos\\fp-pract-tasks-1920-Viktorsmg\\03\\src\\Instances.hs"