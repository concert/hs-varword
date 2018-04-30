module Data.ByteString.Builder.VarWordSpec where

import Test.Hspec

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Word

import Data.ByteString.Builder.VarWord (varWordBe, denseVarWordBe, varWordLe)

spec :: Spec
spec = do
  varWordSpec "Big-endian chunked builder"
    varWordBe "\135\196\64"
  varWordSpec "Big-endian dense chunked builder"
    denseVarWordBe "\134\195\64"
  varWordSpec "Little-endian chunked builder"
    varWordLe "\192\196\7"

varWordSpec :: String -> (Word32 -> Builder) -> ByteString -> Spec
varWordSpec s b expected = describe s $ do
  it "should serialise a known number" $ do
    LBS.toStrict (Builder.toLazyByteString $ b 123456)
    `shouldBe` expected
