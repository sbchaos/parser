module JSON.JSONSpec where

import Test.Hspec
import Test.QuickCheck

import Text.ParserCombinators.Parsec
import JSON.JSON
import Data.Either


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "JSON" $
    describe "parses json" $ do
      it "parses null json" $
        parse jsonValue "test" "null" `shouldBe` Right JNull
      it "parses number" $
        parse jsonValue "test" "123" `shouldBe` Right (JNumber 123)
      it "parses strings as json" $
        parse jsonValue "test" "\"Hello\"" `shouldBe` Right (JString "Hello")
      it "parses boolean value" $
        parse jsonValue "test" "true" `shouldBe` Right (JBool True)
      it "parses array" $
        parse jsonValue "test" "[true, false, true]" `shouldBe` Right (JArray [JBool True, JBool False, JBool True])
      it "parses objects" $
        parse jsonValue "test" "{\"name\":\"json\"}" `shouldBe` Right (JObject [("name", JString "json")])

      it "parses nested json structure" $
        parse jsonValue "test" "{\"name\":\"json\",\"age\":24,\"old\":true}" `shouldBe` Right (JObject [("name", JString "json"), ("age", JNumber 24), ("old", JBool True)])
