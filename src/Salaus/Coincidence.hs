{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Functionality related to coincidence-counting.

module Salaus.Coincidence where

import Data.Semigroup (Sum(..))
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat)

import Data.Portray (Portray)
import Data.Wrapped (Wrapped(..))

import Salaus.Distribution
import Salaus.Symbol

square :: Num a => a -> a
square x = x * x

-- Raw stats on Bernoulli trials: success count and trial count.
--
-- Like Ratio, but not normalized.
data Trials = Int :/ Int
  deriving (Eq, Ord, Read, Show, Generic)
  deriving Portray via Wrapped Generic Trials

instance Semigroup Trials where
  (x :/ y) <> (z :/ w) = (x + z) :/ (y + w)

instance Monoid Trials where
  mempty = 0 :/ 0

getRate :: Trials -> Float
getRate (c :/ t) = fromIntegral c / fromIntegral t

-- gamma rate-of-coincidence: the expected rate of coincidence of text
-- generated randomly from the distribution.
gammaRoC' :: Histogram a -> Trials
gammaRoC' h = getSum (foldMap square h) :/ getSum (square (sum h))

gammaRoC :: Histogram a -> Float
gammaRoC = getRate . gammaRoC'

ones :: KnownNat n => Histogram (Symbol n)
ones = histogram alphabet

-- rate of coincidence of uniform random data with N symbols
uniformRoC :: forall n. KnownNat n => Float
uniformRoC = gammaRoC (ones @n)

-- gamma index of coincidence
gammaIoC :: forall n. KnownNat n => Histogram (Symbol n) -> Float
gammaIoC h = gammaRoC h / uniformRoC @n

-- delta rate-of-coincidence: the rate of coincidences of pairs drawn without
-- replacement from a sample.
deltaRoC' :: Foldable f => f (Sum Int) -> Trials
deltaRoC' h = getSum (foldMap (\x -> x * (x - 1)) h) :/ getSum (n * (n - 1))
 where
  n = sum h

deltaRoC :: Foldable f => f (Sum Int) -> Float
deltaRoC = getRate . deltaRoC'

-- delta index of coincidence
deltaIoC :: forall n. KnownNat n => Histogram (Symbol n) -> Float
deltaIoC h = deltaRoC h / uniformRoC @n

kappaRoC :: [Histogram a] -> Float
kappaRoC = getRate . foldMap deltaRoC'

kappaIoC :: forall n. KnownNat n => [Histogram (Symbol n)] -> Float
kappaIoC h = kappaRoC h / uniformRoC @n

