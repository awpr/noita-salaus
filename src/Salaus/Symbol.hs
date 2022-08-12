{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Salaus.Symbol
  ( Symbol(..), intToSymbol, sym, alphabet, fromString
  ) where

import Prelude hiding ((.))

import Control.Category ((.))
import Data.Coerce (coerce)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import GHC.Exts (IsList(..))
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, natVal)

import Data.Text qualified as T
import Data.Text (Text)

import Data.Fin.Int (Fin, finToInt, fin, tryFin, enumFin)
import Data.Portray
import Data.Vec.Short qualified as V
import Data.Vec.Short (Vec)
import Data.Type.Attenuation (Attenuable(..), coercible)


newtype Symbol n = Symbol { getSymbol :: Fin n }
  deriving (Eq, Ord, Enum, Show)

alphabet :: KnownNat n => [Symbol n]
alphabet = coerce enumFin

instance Attenuable (Fin n) a => Attenuable (Symbol n) a where
  attenuation = attenuation @(Fin n) . coercible

intToSymbol :: (KnownNat n, HasCallStack) => Int -> Symbol n
intToSymbol = Symbol . fin

symbolGlyphs :: String
symbolGlyphs =
  concat
    [ ['a'..'z']
    , ['A'..'Z']
    , ['0'..'9']
    , "αβγδεζηθικλμνξοπρστυφχψω"
    ]

type NumGlyphs = 86

-- An arbitrary sequence of distinct printable glyphs to represent characters;
-- for @Symbol n@, the first @n@ will be used.
glyphTable :: Vec NumGlyphs Char
glyphTable = V.fromList symbolGlyphs

glyphValues :: M.Map Char Int
glyphValues = M.fromList $ zip (toList glyphTable) [0..]

trySym :: KnownNat n => Char -> Maybe (Symbol n)
trySym c = fmap Symbol . tryFin =<< M.lookup c glyphValues

sym :: (KnownNat n, HasCallStack) => Char -> Symbol n
sym = fromMaybe (error "sym: unrecognized character") . trySym

toText' :: [Symbol n] -> Text
toText' xs = T.pack $ map ((glyphTable V.!) . fin . finToInt . getSymbol) xs

instance KnownNat n => Portray (Symbol n) where
  portray (Symbol x)
    | natVal @n Proxy <= natVal @NumGlyphs Proxy = LitChar (glyphTable V.! fin (finToInt x))
    | otherwise = LitInt (toInteger (finToInt x))

  portrayList xs
    | natVal @n Proxy <= natVal @NumGlyphs Proxy = Apply (Name "fromString") [LitStr (toText' xs)]
    | otherwise = portray xs

blame :: (a -> Maybe b) -> a -> Either a b
blame f x = maybe (Left x) Right (f x)

fromString :: KnownNat n => String -> [Symbol n]
fromString =
  either
    (error . showString "fromString: unrecognized character " . show)
    id .
  traverse (blame trySym)
