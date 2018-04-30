{- | Implementations of 'BoundedPrim' for several variable-length integer
     encodings. Rather than use these directly, consider the higher level
     "Data.ByteString.Builder.VarNum" 'Data.ByteString.Builder.Builder'
     interface.
-}
module Data.ByteString.Builder.Prim.VarWord
  ( varWordBe, denseVarWordBe
  , varWordLe
  ) where

import Data.Bits (Bits, shiftR, setBit, (.&.))
import Data.ByteString.Builder.Prim.Internal (BoundedPrim, boudedPrim)
import Data.Word
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable, poke, sizeOf)


-- | 'BoundedPrim' for big-endian (most significant chunk first) variable length
--   encoding
varWordBe :: forall a. (Bits a, Integral a, Storable a) => BoundedPrim a
varWordBe = boundedPrim (sizeOf (undefined :: a)) go
  where
    go :: a -> Ptr Word8 -> IO (Ptr Word8)
    go a ptr = write7BitChunks (chunkWordBe a) ptr

-- | 'BoundedPrim' for big-endian (most significant chunk first) variable length
--   encoding where the continuation bit is also reused to pack information
--   slightly more densely.
denseVarWordBe :: forall a. (Bits a, Integral a, Storable a) => BoundedPrim a
denseVarWordBe = boundedPrim (sizeOf (undefined :: a)) go
  where
    go :: a -> Ptr Word8 -> IO (Ptr Word8)
    go a ptr = write7BitChunks (denseChunkWordBe a) ptr

-- | 'BoundedPrim' for little-endian (least significant chunk first) variable
--   length encoding
varWordLe :: forall a. (Bits a, Integral a, Storable a) => BoundedPrim a
varWordLe = boundedPrim (sizeOf (undefined :: a)) go
  where
    go :: a -> Ptr Word8 -> IO (Ptr Word8)
    go a ptr = write7BitChunks (chunkWordLe a) ptr

chunkWordBe :: (Bits a, Integral a) => a -> [Word8]
chunkWordBe = reverse . chunkWordLe' 0

denseChunkWordBe :: (Bits a, Integral a) => a -> [Word8]
denseChunkWordBe = reverse . chunkWordLe' 1

chunkWordLe :: (Bits a, Integral a) => a -> [Word8]
chunkWordLe = chunkWordLe' 0

chunkWordLe' :: (Bits a, Integral a) => a -> a -> [Word8]
chunkWordLe' contDec a =
  let
    highBits = shiftR a 7
    lowBits = a .&. 0b01111111
  in
    fromIntegral lowBits :
    if highBits == 0 then [] else chunkWordLe' contDec (highBits - contDec)

write7BitChunks :: [Word8] -> Ptr Word8 -> IO (Ptr Word8)
write7BitChunks [] ptr = return ptr
write7BitChunks (c:cs) ptr = do
  poke ptr $ case cs of
    [] -> c
    _ -> setBit c 7
  write7BitChunks cs $ plusPtr ptr 1

-- Fix a typo from the Builder.Prim.Internal:
boundedPrim :: Int -> (a -> Ptr Word8 -> IO (Ptr Word8)) -> BoundedPrim a
boundedPrim = boudedPrim
