{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-} -- shush a warning on simplifiable constraints
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad (replicateM)
import Data.Bifunctor (first)
import Data.Char (chr, ord, isSpace)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.IntMap qualified as IM
import Data.List (mapAccumL, transpose, tails)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Sum(..), Max(..), Arg(..))
import Data.Vector qualified as V
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, natVal)
import System.Process (callCommand)
import System.Random (randomRIO)

import Codec.Picture.Png (writePng)
import Codec.Picture.Types (Pixel8, Image, generateImage)
import System.Random.Shuffle (shuffleM)
import Data.Fin.Int (Fin, (-%), (+%), enumFin, fin)
import Data.IntMap.Keyed qualified as KM
import Data.RLE qualified as RLE
import Data.Type.Attenuation (type (⊆))
import Data.Portray (Portray)
import Data.Wrapped (Wrapped(..))

import Data
import Salaus.Distribution
import Salaus.Symbol

_ = messages

square :: Num a => a -> a
square x = x * x

-- Like Ratio, but not normalized.
data RateStats = Int :/ Int
  deriving (Eq, Ord, Read, Show, Generic)
  deriving Portray via Wrapped Generic RateStats

instance Semigroup RateStats where
  (x :/ y) <> (z :/ w) = (x + z) :/ (y + w)

instance Monoid RateStats where
  mempty = 0 :/ 0

getRate :: RateStats -> Float
getRate (c :/ t) = fromIntegral c / fromIntegral t

-- gamma rate-of-coincidence: the expected rate of coincidence of text
-- generated randomly from the distribution.
gammaRoC' :: Histogram a -> RateStats
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
deltaRoC' :: Foldable f => f (Sum Int) -> RateStats
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

deltas :: [Int] -> [Int]
deltas [] = []
deltas (x0 : xs0) = go x0 xs0
 where
  go _ [] = []
  go x (y : ys) = ((y - x) `mod` 82) : go y ys

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (y, ys) = splitAt n xs in y : chunksOf n ys

stripesOf :: Int -> [a] -> [[a]]
stripesOf n = transpose . chunksOf n

window :: Int -> [a] -> [[a]]
window n xs = takeWhile ((==n) . length) . transpose . take n . tails $ xs

sumCounts :: a ⊆ Int => [Histogram a] -> Histogram a
sumCounts = foldr (KM.unionWith (+)) KM.empty

-- A list containing at each index the frequency distributions of symbols at
-- that index within their respective messages.
positionalFrequencies :: a ⊆ Int => [[a]] -> [Histogram a]
positionalFrequencies = map sumCounts . transpose . map (map one)

stripe :: a ⊆ Int => Int -> [Histogram a] -> [Histogram a]
stripe k = map sumCounts . stripesOf k

chunk :: a ⊆ Int => Int -> [Histogram a] -> [Histogram a]
chunk k = map sumCounts . chunksOf k

-- A list of frequency distributions by position % n.
--
-- This would contain monoalphabetic distributions if the cipher rotates
-- between n alphabets every position, repeating with period n
modPositionalFrequencies :: a ⊆ Int => Int -> [[a]] -> [Histogram a]
modPositionalFrequencies k = stripe k . positionalFrequencies

divPositionalFrequencies :: a ⊆ Int => Int -> [[a]] -> [Histogram a]
divPositionalFrequencies k = chunk k . positionalFrequencies

-- A grid of ASCII digits showing a frequency table.
--
-- If frequencies are out of [0, 10), shows weird characters rather than
-- misaligning the grid.
toDigitGrid :: KnownNat n => [Histogram (Symbol n)] -> String
toDigitGrid = unlines . map toRow
 where
  toRow hist =
    [ case KM.lookup i hist of Nothing -> ' '; Just n -> chr (getSum n + ord '0')
    | i <- alphabet
    ]

divides :: Int -> Int -> Bool
x `divides` y = y `mod` x == 0

ruler :: Int -> Int -> Pixel8
ruler scale x
  | (50 * scale) `divides` x = 255
  | (10 * scale) `divides` x = 127
  | scale `divides` x = 63
  | otherwise = 0

type ImgFn a = Int -> Int -> a

addRuler :: Int -> ImgFn Pixel8 -> ImgFn Pixel8
addRuler scale f x y
  | y < scale = ruler scale (x - scale)
  | x < scale = ruler scale (y - scale)
  | (10 * scale) `divides` (x - scale) = 127
  | (10 * scale) `divides` (y - scale) = 127
  | otherwise = f (x - scale) (y - scale)

rescale :: Int -> ImgFn Pixel8 -> ImgFn Pixel8
rescale scale f x y = f (x `div` scale) (y `div` scale)

toHeatmap :: forall n. KnownNat n => Int -> [Histogram (Symbol n)] -> Image Pixel8
toHeatmap scale rows =
  generateImage
    (addRuler scale $ rescale scale mkPixel)
    (scale * (1 + k))
    (scale * (1 + V.length vecRows))
 where
  k = fromIntegral $ natVal @n Proxy
  mkPixel x y =
    freqBrightness (vecSums V.! y) $
      KM.findWithDefault 0 (intToSymbol x) (vecRows V.! y)

  quantize = fromIntegral . min 255 . floor @_ @Integer . (*256)

  sigmoid x = 1 / (1 + exp (- 2 * (x - 1)))

  freqBrightness total ct = quantize $ sigmoid $ fromIntegral (k * getSum ct) / total

  vecRows = V.fromList rows

  vecSums :: V.Vector Float
  vecSums = fromIntegral . getSum . sum <$> vecRows

viewImage :: Image Pixel8 -> IO ()
viewImage img = do
  writePng "/tmp/test.png" img
  callCommand "feh /tmp/test.png"

dotProduct :: a ⊆ Int => Histogram a -> Histogram a -> Int
dotProduct xs ys = getSum $ sum $ KM.intersectionWith (*) xs ys

similarityMatrix :: a ⊆ Int => [Histogram a] -> [[Int]]
similarityMatrix countss = countss <&> \counts -> dotProduct counts <$> countss

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

flattenedUpperTriangle :: [[Int]] -> [Int]
flattenedUpperTriangle mat = concat $
  (map.map) (snd . snd) $
  map (filter (\ (i, (j, _)) -> i > j)) $
  map (sequenceA . fmap enumerate) $
  enumerate mat

matrixStats :: [[Int]] -> (Float, Float)
matrixStats mat = (mean, stddev)
 where
  fut = flattenedUpperTriangle mat

  total, n, mean, stddev :: Float
  total = fromIntegral $ sum fut
  n = fromIntegral $ length fut
  mean = total / n
  stddev = sqrt $ sum [d * d | x <- fut, let d = fromIntegral x - mean] / (n - 1)

-- Like liftA2 f x x, but excluding swapped pairs
choose2 :: (a -> a -> b) -> [a] -> [b]
choose2 _ [] = []
choose2 f (x : xs) = map (f x) xs ++ choose2 f xs

matrixImg :: Int -> [[Int]] -> Image Pixel8
matrixImg scale mat =
  generateImage
    (addRuler scale $ rescale scale mkPixel)
    (scale * (1 + w))
    (scale * (1 + w))
 where
  mkPixel x y =
    floor $ min bound $ bound * (fromIntegral $ matVec V.! x V.! y) / saturationPoint

  bound = 255

  (mean, stddev) = matrixStats mat
  saturationPoint = mean + stddev * 10

  matVec = V.fromList $ V.fromList <$> mat
  w = V.length matVec

adjacentPositionDeltas :: KnownNat n => [[Symbol n]] -> Histogram (Fin n)
adjacentPositionDeltas ms =
  histogram $
    concatMap
      (\m -> zipWith (\x y -> (getSymbol y -% getSymbol x)) m (tail  m))
      ms

bigrams :: [a] -> [(a, a)]
bigrams xs = zipWith (,) xs (tail xs)

writeTsv :: (Show a, Show b) => String -> [(a, b)] -> IO ()
writeTsv nm h = writeFile nm $ unlines $ map formatOne h
 where
  formatOne (x, f) = shows x $ showString "\t" $ show f

writeTsvIM :: Show a => String -> IM.IntMap a -> IO ()
writeTsvIM nm = writeTsv nm . IM.toList

writeTsvM :: (Show a, Show b) => String -> M.Map a b -> IO ()
writeTsvM nm = writeTsv nm . M.toList

-- Ways of getting corpuses ----

-- Shuffle all the symbols in a corpus randomly, across messages, while
-- preserving the input messages' lengths.
shuffleCorpus :: [[a]] -> IO [[a]]
shuffleCorpus ms =
  shuffleM (concat ms) <&> \dat ->
    snd $ mapAccumL (\dat' n -> swap $ splitAt n dat') dat lengths
 where
  swap (x, y) = (y, x)
  lengths = map length ms

-- Generate a random corpus of 'n' messages of length 'n'
randomCorpus :: forall n. KnownNat n => Int -> Int -> IO [[Symbol n]]
randomCorpus n m =
  replicateM n $ replicateM m $
    intToSymbol <$> randomRIO (0, fromIntegral $ natVal @n Proxy - 1)

asciiToSymbol :: Char -> Symbol 96
asciiToSymbol c = intToSymbol (ord c - 32)

-- Read a corpus linewise from a text file, stripping blank lines.
readCorpus :: String -> IO [[Symbol 96]]
readCorpus nm = readFile nm <&> map (map asciiToSymbol) . filter (not . all isSpace) . lines

argMax :: (HasCallStack, Foldable f, Ord b) => (a -> b) -> f a -> a
argMax f = (\ (Arg _ x) -> x) . getMax . fromMaybe (error "") . foldMap (\x -> Just $ Max $ Arg (f x) x)

argMaxFin :: (KnownNat n, Ord a) => (Fin n -> a) -> Fin n
argMaxFin f = argMax f enumFin

dotProductMod :: KnownNat n => Fin n -> Histogram (Symbol n) -> Histogram (Symbol n) -> Int
dotProductMod k h1 h2 =
  getSum $
  foldMap
    (\ i ->
      (KM.findWithDefault 0 (Symbol i) h1) *
      (KM.findWithDefault 0 (Symbol (i -% k)) h2))
    enumFin

bestAlignment :: KnownNat n => Histogram (Symbol n) -> Histogram (Symbol n) -> Fin n
bestAlignment h1 h2 = argMaxFin (\k -> dotProductMod k h1 h2)

rotateMod :: KnownNat n => Fin n -> Histogram (Symbol n) -> Histogram (Symbol n)
rotateMod k = KM.fromList . map (first (\ (Symbol x) -> Symbol (x +% k))) . KM.toList

alignmentSearch :: forall n. KnownNat n => Int -> [Histogram (Symbol n)] -> ([Fin n], Histogram (Symbol n))
alignmentSearch fuel hs = go fuel (IM.fromList [(i, fin 0) | i <- [0 .. l - 1]]) (fold hs)
 where
  hsVec = V.fromList hs
  l = V.length hsVec

  go :: Int -> IM.IntMap (Fin n) -> Histogram (Symbol n) -> ([Fin n], Histogram (Symbol n))
  go 0 offs overall = ([IM.findWithDefault (fin 0) i offs | i <- [0 .. l - 1]], overall)
  go n offs overall =
    let i = n `mod` l
        h = hsVec V.! i
        h' = rotateMod (offs IM.! i) h
        sansH = KM.unionWith (+) overall (negate <$> h')
        k' = bestAlignment sansH h
        offs' = IM.insert i k' offs
        overall' = KM.unionWith (+) sansH (rotateMod k' h)
    in  go (n-1) offs' overall'

-- Random stuff ----

runValue :: RLE.Run a -> a
runValue (_ RLE.:>< x) = x

leastFactor :: Int -> Maybe Int
leastFactor n = listToMaybe $ [x | x <- takeWhile ((<= n) . square) [2..], x `divides` n]

-- slow, but quick and dirty impl.
primeFactors :: Int -> [Int]
primeFactors n = case leastFactor n of
  Nothing -> [n]
  Just x -> x : primeFactors (n `div` x)
