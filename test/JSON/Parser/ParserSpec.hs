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
    describe "lexme combinator" $
      it "removes leading and trailing whitespace" $
        parse (lexme matchTrue) "description" "\t  true \n" `shouldBe` Right True
