module Data.ByteString.Builder.Prim.VarWord
  ( varWordBe, denseVarWordBe
  , varWordLe
  ) where

import Data.Bits (Bits, shiftR, setBit, (.&.))
import Data.ByteString.Builder.Prim.Internal (BoundedPrim, boudedPrim)
import Data.Word
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable, poke, sizeOf)


varWordBe :: forall a. (Bits a, Integral a, Storable a) => BoundedPrim a
varWordBe = boundedPrim (sizeOf @a undefined) go
  where
    go :: a -> Ptr Word8 -> IO (Ptr Word8)
    go a ptr = write7BitChunks (chunkWordBe a) ptr

denseVarWordBe :: forall a. (Bits a, Integral a, Storable a) => BoundedPrim a
denseVarWordBe = boundedPrim (sizeOf @a undefined) go
  where
    go :: a -> Ptr Word8 -> IO (Ptr Word8)
    go a ptr = write7BitChunks (denseChunkWordBe a) ptr

varWordLe :: forall a. (Bits a, Integral a, Storable a) => BoundedPrim a
varWordLe = boundedPrim (sizeOf @a undefined) go
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
