module JSON.Parser.ParserSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Text.ParserCombinators.Parsec
import JSON.Parser.Parser
import Data.Either


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Parser" $ do

    describe "matchTrue" $ do
      it "matches the string true" $
        parse matchTrue "test" "true" `shouldBe` Right True
      it "errors whin string is not true" $
        isLeft (parse matchTrue "test" "blue") `shouldBe` True

    describe "mathFalse" $ do
      it "matches the string false" $
        parse matchFalse "test" "false" `shouldBe` Right False
      it "errors whin string is not true" $
        isLeft (parse matchFalse "test" "blue") `shouldBe` True

    describe "bool" $ do
      it "matches true" $
        parse bool "test" "true\t" `shouldBe` Right True
      it "matches false" $
        parse bool "test" "false " `shouldBe` Right False

    describe "lexme combinator" $
      it "removes leading and trailing whitespace" $
        parse (lexme matchTrue) "test" "\t  true \n" `shouldBe` Right True

    describe "strings" $ do
      it "extrcts string from text" $
        parse strings "test" "\"Hello\"" `shouldBe` Right "Hello"
      it "extracts strings from text" $
        parse strings "test" "\"Hello World\"" `shouldBe` Right "Hello World"

    describe "numbers" $ do
      it "extracts the number" $
        parse numbers "test" "1234" `shouldBe` Right 1234
      it "extracts the negative numbers" $
        parse numbers "test" "-123" `shouldBe` Right (-123.0)

    describe "parseNull" $
      it "matches null value" $
        parse parseNull "test" "null" `shouldBe` Right "null"