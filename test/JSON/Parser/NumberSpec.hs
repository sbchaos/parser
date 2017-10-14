module JSON.Parser.NumberSpec where

import Test.Hspec
import Test.QuickCheck

import Text.ParserCombinators.Parsec
import JSON.Parser.Number
import Data.Either


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Parser" $ do
    it "parses number properly" $
      parse number "test" "1234" `shouldBe` Right 1234
    it "parses number with exponent" $
      parse number "test" "2.3956185979e+06" `shouldBe` Right 2395618.5979
    it "parses number with exponent" $
          parse number "test" "20.3956e-02" `shouldBe` Right 0.203956