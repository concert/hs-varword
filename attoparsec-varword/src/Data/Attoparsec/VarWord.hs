{- |
Description: "Attoparsec" 'Parser's for varible-length-encoded integers

We define "Attoparsec" 'Parser's for several different variable-length integer
encoding schemes. These schemes are useful for transmitting numerical data
in a more compact form than would be available from simple big- or little-endian
encoding of the values.
-}

module Data.Attoparsec.VarWord
  ( varWordBe, denseVarWordBe
  , varWordLe
  ) where

import Data.Attoparsec.ByteString (Parser)
import Data.Bits (Bits)

import Data.Attoparsec.VarWord.Internal (varWordBe', varWordLe')


-- |  Decode a big-endian (most significant byte first), variable-length encoded
--    value.
--
--    > +-- Keep reading
--    > |        +-- Keep reading
--    > |        |        +-- Stop
--    > v        v        v
--    > 10000001 10010000 00100000
--    >  |<--->|  |<--->|  |<--->|
--    >  Data(A)  Data(B)  Data(C)
--    >
--    >   ==   1 0010000 0100000 == 18464
--    >      (A) (B)     (C)
--
--    >>> parseOnly varWordBe "\129\144\32"
--    Right 18464
--
varWordBe :: (Bits a, Integral a) => Parser a
varWordBe = varWordBe' 0 0

-- | Decode a big-endian (least significant byte first), variable length
--   encoded value where the continuation byte is used to carry data also.
--
--    > +-- Keep reading
--    > |        +-- Keep reading
--    > |        |        +-- Stop
--    > v        v        v
--    > 10000001 10010000 00100000
--    >  |<--->|  |<--->|  |<--->|
--    >  Data(A)  Data(B)  Data(C)
--    >
--    >   ==  10 0010001 0100000 == 34976
--    >    (A+1) (B+1)   (C)
--
--    >>> parseOnly denseVarWordBeP "\129\144\32"
--    Right 34976
--
denseVarWordBe :: (Bits a, Integral a) => Parser a
denseVarWordBe = varWordBe' 1 0

-- | Decode a little-endian (least significant byte first), variable length
--   encoded value.
--
--    > +-- Keep reading
--    > |        +-- Keep reading
--    > |        |        +-- Stop
--    > v        v        v
--    > 10000001 10010000 00100000
--    >  |<--->|  |<--->|  |<--->|
--    >  Data(A)  Data(B)  Data(C)
--    >
--    >   == 100000 0010000 0000001 == 526337
--    >      (C)    (B)     (A)
--
--    >>> parseOnly varWordLe "\129\144\32"
--    Right 526337
--
varWordLe :: (Bits a, Integral a) => Parser a
varWordLe = varWordLe' 0 0
