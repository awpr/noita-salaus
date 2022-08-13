{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-} -- shush a warning on simplifiable constraints
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Salaus.Distribution where

import Data.Coerce (coerce)
import Data.Map.Strict qualified as M
import Data.Semigroup (Sum(..))

import Data.IntMap.Keyed qualified as KM
import Data.Type.Attenuation (type (⊆))

type Histogram a = KM.IntMap a (Sum Int)
type Distribution a = KM.IntMap a Float

-- An IntMap containing the single mapping i => 1
one :: a ⊆ Int => a -> Histogram a
one i = KM.singleton i 1

insertm :: (a ⊆ Int, Monoid m) => a -> m -> KM.IntMap a m -> KM.IntMap a m
insertm k v m = KM.alter (Just . \case { Nothing -> v; Just x -> x <> v }) k m

insertm' :: (Ord a, Monoid m) => a -> m -> M.Map a m -> M.Map a m
insertm' k v m = M.alter (Just . \case { Nothing -> v; Just x -> x <> v }) k m

histogram :: a ⊆ Int => [a] -> Histogram a
histogram xs = coerce $ foldr (\x m -> insertm x (Sum @Int 1) m) KM.empty xs

histogram' :: Ord a => [a] -> M.Map a (Sum Int)
histogram' xs = foldr (\x m -> insertm' x (Sum @Int 1) m) M.empty xs

toDistribution :: (Functor f, Foldable f) => f (Sum Int) -> f Float
toDistribution h = fmap (\ (Sum x) -> fromIntegral x / n) h
 where
  n = fromIntegral $ getSum $ sum h

freqs :: a ⊆ Int => [a] -> Distribution a
freqs = toDistribution . histogram

rfreqs :: a ⊆ Int => Float -> [a] -> Distribution a
rfreqs k xs = fmap (*k) $ freqs xs
