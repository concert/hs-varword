module Data.Attoparsec.VarWordSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Attoparsec.ByteString (Parser, parseOnly)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.VarWord as Builder

import Data.Attoparsec.VarWord (varWordBe, denseVarWordBe, varWordLe)

spec :: Spec
spec = do
  varWordSpec "Big-endian chunked number parser"
    varWordBe Builder.varWordBe 18464
  varWordSpec "Big-endian dense chunked number parser"
    denseVarWordBe Builder.denseVarWordBe 34976
  varWordSpec "Little-endian chunked number parser"
    varWordLe Builder.varWordLe 526337


varWordSpec :: String -> Parser Word32 -> (Word32 -> Builder) -> Word32 -> Spec
varWordSpec s p b expected = describe s $ do
  it "should parse a known bytestring" $ do
    parseOnly p (BS.pack [0b10000001, 0b10010000, 0b00100000])
      `shouldBe` Right expected
  it "should parse any valid single byte" $ forAll (choose (0, 127)) $ \word8 ->
    parseOnly p (BS.singleton word8) `shouldBe` Right (fromIntegral word8)
  it "should roundtrip any valid bytestring" $ property $ \word32 ->
    let
      encoded = LBS.toStrict $ Builder.toLazyByteString $ b word32
    in
      parseOnly p encoded `shouldBe` Right word32
