module Data.ByteString.Builder.VarWord
  ( varWordBe, denseVarWordBe
  , varWordLe
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder.Prim (primBounded)
import Data.Bits (Bits)

import Foreign.Storable (Storable)

import qualified Data.ByteString.Builder.Prim.VarWord as Prim


varWordBe :: (Bits a, Integral a, Storable a) => a -> Builder
varWordBe = primBounded Prim.varWordBe

denseVarWordBe :: (Bits a, Integral a, Storable a) => a -> Builder
denseVarWordBe = primBounded Prim.denseVarWordBe

varWordLe :: (Bits a, Integral a, Storable a) => a -> Builder
varWordLe = primBounded Prim.varWordLe
