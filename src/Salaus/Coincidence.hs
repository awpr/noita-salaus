{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Functionality related to coincidence-counting.

module Salaus.Coincidence where

import Data.Functor ((<&>))
import Data.Semigroup (Sum(..))
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat)

import Data.Portray (Portray(..), Portrayal(Binop), Infixity(..), Assoc(..))
import Data.Wrapped (Wrapped(..))

import Salaus.Distribution
import Salaus.Symbol

square :: Num a => a -> a
square x = x * x

-- A symmetric confidence interval of 'a' represented as estimate and margin.
data Erroneous a = Erroneous { errEstimate :: !a, errError :: !a }
  deriving (Eq, Ord, Read, Show, Generic, Functor)

-- | Construct an 'Erroneous' from estimate and error, normalizing with 'abs'.
-- Type ± in vim with ^K+-.
infix 1 ±
(±) :: Num a => a -> a -> Erroneous a
x ± e = Erroneous x (abs e)

instance Portray a => Portray (Erroneous a) where
  portray (Erroneous x e) =
    Binop "±" (Infixity AssocNope 1) (portray x) (portray e)

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

-- The Wald-style confidence interval of the success probability.
waldInterval :: Trials -> Erroneous Float
waldInterval (s' :/ n') = p ± z * sqrt (p * (1 - p) / n)
 where
  s = fromIntegral s'
  n = fromIntegral n'
  p = s / n
  z = 1.96 -- probit corresponding to a 0.95 confidence level

getRate :: Trials -> Float
getRate = errEstimate . waldInterval

-- gamma rate-of-coincidence: the expected rate of coincidence of text
-- generated randomly from the distribution.
gammaRoC' :: Histogram a -> Trials
gammaRoC' h = getSum (foldMap square h) :/ getSum (square (sum h))

gammaRoC :: Histogram a -> Erroneous Float
gammaRoC = waldInterval . gammaRoC'

ones :: KnownNat n => Histogram (Symbol n)
ones = histogram alphabet

-- rate of coincidence of uniform random data with N symbols
uniformRoC :: forall n. KnownNat n => Float
uniformRoC = errEstimate $ gammaRoC (ones @n)

-- gamma index of coincidence
gammaIoC :: forall n. KnownNat n => Histogram (Symbol n) -> Erroneous Float
gammaIoC h = gammaRoC h <&> (/ uniformRoC @n)

-- delta rate-of-coincidence: the rate of coincidences of pairs drawn without
-- replacement from a sample.
deltaRoC' :: Foldable f => f (Sum Int) -> Trials
deltaRoC' h = getSum (foldMap (\x -> x * (x - 1)) h) :/ getSum (n * (n - 1))
 where
  n = sum h

deltaRoC :: Foldable f => f (Sum Int) -> Erroneous Float
deltaRoC = waldInterval . deltaRoC'

-- delta index of coincidence
deltaIoC :: forall n. KnownNat n => Histogram (Symbol n) -> Erroneous Float
deltaIoC h = deltaRoC h <&> (/ uniformRoC @n)

kappaRoC :: [Histogram a] -> Erroneous Float
kappaRoC = waldInterval . foldMap deltaRoC'

kappaIoC :: forall n. KnownNat n => [Histogram (Symbol n)] -> Erroneous Float
kappaIoC h = kappaRoC h <&> (/ uniformRoC @n)
