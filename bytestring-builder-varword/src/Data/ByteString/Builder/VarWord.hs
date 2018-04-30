{- | 'Builder's for several variable-length integer encoding schemes.
-}
module Data.ByteString.Builder.VarWord
  ( varWordBe, denseVarWordBe
  , varWordLe
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder.Prim (primBounded)
import Data.Bits (Bits)

import Foreign.Storable (Storable)

import qualified Data.ByteString.Builder.Prim.VarWord as Prim


-- | Produce a 'Builder' to encode an integer with a variable-length (chunked)
--   encoding. 7-bit chunks are produced in big-endian (most significant chunk
--   first) order, and each is prefixed with a continuation bit.
--
--    > 18464 ==  1 0010000 0100000 ==
--    >         (A) (B)     (C)
--    >
--    > +-- Keep reading
--    > |        +-- Keep reading
--    > |        |        +-- Stop
--    > v        v        v
--    > 10000001 10010000 00100000
--    >  |<--->|  |<--->|  |<--->|
--    >    (A)      (B)      (C)
--
--    >>> Builder.toLazyByteString $ varWordBe (18464 :: Word32)
--    "\129\144\32"
varWordBe :: (Bits a, Integral a, Storable a) => a -> Builder
varWordBe = primBounded Prim.varWordBe

-- | Produce a 'Builder' to encode an integer with a variable-length (chunked)
--   encoding. 7-bit chunks are produced in big-endian (most significant chunk
--   first) order, each is prefixed with a continuation bit, and the
--   continuation bit carries a further bit of information to reduce the size of
--   the resulting encoding.
--
--    > 34976  == 10 0010001 0100000 == 34976
--    >          (A) (B)     (C)
--    >
--    > +-- Keep reading
--    > |        +-- Keep reading
--    > |        |        +-- Stop
--    > v        v        v
--    > 10000001 10010000 00100000
--    >  |<--->|  |<--->|  |<--->|
--    >   (A-1)    (B-1)     (C)
--
--    >>> Builder.toLazyByteString $ denseVarWordBe (34976 :: Word32)
--    "\129\144\32"
denseVarWordBe :: (Bits a, Integral a, Storable a) => a -> Builder
denseVarWordBe = primBounded Prim.denseVarWordBe

-- | Produce a 'Builder' to encode an integer with a varible-length (chunked)
--   encoding. 7-bit chunks are produced in little-endian (least significant
--   chunk first) order, and each is prefixed with a continuation bit.
--
--    > 526337 == 100000 0010000 0000001 ==
--    >           (A)    (B)     (C)
--    >
--    > +-- Keep reading
--    > |        +-- Keep reading
--    > |        |        +-- Stop
--    > v        v        v
--    > 10000001 10010000 00100000
--    >  |<--->|  |<--->|  |<--->|
--    >    (C)      (B)      (A)
--
--    >>> Builder.toLazyByteString $ varWordLe (526337 :: Word32)
--    "\129\144\32"
varWordLe :: (Bits a, Integral a, Storable a) => a -> Builder
varWordLe = primBounded Prim.varWordLe
