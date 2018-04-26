module Data.Attoparsec.VarWordSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Attoparsec.ByteString (Parser, parseOnly)
import qualified Data.ByteString as BS
import Data.Word

import Data.Attoparsec.VarWord (varWordBe, denseVarWordBe, varWordLe)

spec :: Spec
spec = do
  varNumSpec "Big-endian chunked number parser" varWordBe 18464
  varNumSpec "Big-endian dense chunked number parser" denseVarWordBe 34976
  varNumSpec "Little-endian chunked number parser" varWordLe 526337


varNumSpec :: String -> Parser Word32 -> Word32 -> Spec
varNumSpec s p expected = describe s $ do
  it "should parse a single byte" $ forAll (choose (0, 127)) $ \word8 ->
    parseOnly p (BS.singleton word8) `shouldBe` Right (fromIntegral word8)
  it "should parse a known bytestring" $ do
    parseOnly p (BS.pack [0b10000001, 0b10010000, 0b00100000])
      `shouldBe` Right expected
