module Data.Attoparsec.VarWord.Internal where

import Data.Attoparsec.ByteString (Parser, anyWord8)
import Data.Bits (Bits, shiftL, testBit, (.&.), (.|.))
import Data.Word


msb :: Word8 -> Bool
msb = flip testBit 7

lsbs :: Word8 -> Word8
lsbs = (.&. 0b01111111)


varWordBe' :: (Bits a, Integral a) => a -> a -> Parser a
varWordBe' contInc acc = do
  byte <- anyWord8
  let acc' = shiftL acc 7 .|. fromIntegral (lsbs byte)
  if msb byte
    then varWordBe' contInc (acc' + contInc)
    else return acc'


varWordLe' :: (Bits a, Integral a) => Int -> a -> Parser a
varWordLe' shiftBy acc = do
  byte <- anyWord8
  let acc' = acc .|. shiftL (fromIntegral $ lsbs byte) shiftBy
  if msb byte
    then varWordLe' (shiftBy + 7) acc'
    else return acc'
