{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Data.Char (chr, ord, isSpace)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.IntMap qualified as IM
import Data.List (mapAccumL, transpose, tails)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Sum(..))
import Data.Vector qualified as V
import GHC.TypeNats (KnownNat, natVal)
import System.Process (callCommand)
import System.Random (randomRIO)

import Codec.Picture.Png (writePng)
import Codec.Picture.Types (Pixel8, Image, generateImage)
import System.Random.Shuffle (shuffleM)
import Data.Fin.Int (Fin, (-%))
import Data.IntMap.Keyed qualified as KM
import Data.RLE qualified as RLE
import Data.Type.Attenuation (type (⊆))

import Data
import Salaus.Symbol

_ = messages

type Histogram a = KM.IntMap a Int
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

histogram' :: Ord a => [a] -> M.Map a Int
histogram' xs = coerce $ foldr (\x m -> insertm' x (Sum @Int 1) m) M.empty xs

toDistribution :: (Functor f, Foldable f) => f Int -> f Float
toDistribution h = fmap (\x -> fromIntegral x / n) h
 where
  n = fromIntegral $ sum h

freqs :: a ⊆ Int => [a] -> Distribution a
freqs = toDistribution . histogram

rfreqs :: a ⊆ Int => Float -> [a] -> Distribution a
rfreqs k xs = fmap (*k) $ freqs xs

square :: Num a => a -> a
square x = x * x

data RateStats = RateStats { rsCount :: !Int, rsTests :: !Int }
  deriving Show

instance Semigroup RateStats where
  RateStats x y <> RateStats z w = RateStats (x + z) (y + w)

instance Monoid RateStats where
  mempty = RateStats 0 0

getRate :: RateStats -> Float
getRate (RateStats c t) = fromIntegral c / fromIntegral t

-- gamma rate-of-coincidence: the expected rate of coincidence of text
-- generated randomly from the distribution.
gammaRoC' :: Histogram a -> RateStats
gammaRoC' h = RateStats (getSum $ foldMap (Sum . square) h) (square (sum h))

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
deltaRoC' :: Histogram a -> RateStats
deltaRoC' h = RateStats (getSum $ foldMap (\x -> Sum (x * (x - 1))) h) (n * (n - 1))
 where
  n = sum h

deltaRoC :: Histogram a -> Float
deltaRoC = getRate . deltaRoC'

-- delta index of coincidence
deltaIoC :: forall n. KnownNat n => Histogram (Symbol n) -> Float
deltaIoC h = deltaRoC h / uniformRoC @n

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
    [ case KM.lookup i hist of Nothing -> ' '; Just n -> chr (n + ord '0')
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

  freqBrightness total ct = quantize $ sigmoid $ fromIntegral (k * ct) / total

  vecRows = V.fromList rows

  vecSums :: V.Vector Float
  vecSums = fromIntegral . sum <$> vecRows

viewImage :: Image Pixel8 -> IO ()
viewImage img = do
  writePng "test.png" img
  callCommand "feh test.png"

dotProduct :: a ⊆ Int => Histogram a -> Histogram a -> Int
dotProduct xs ys = sum $ KM.intersectionWith (*) xs ys

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
    intToSymbol <$> randomRIO (0, fromIntegral $ natVal @n Proxy)

-- Read a corpus linewise from a text file, stripping spaces.
readCorpus :: String -> IO [String]
readCorpus nm = readFile nm <&> filter (not . all isSpace) . lines


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
