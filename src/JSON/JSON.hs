module JSON.JSON where

import JSON.Parser.Parser

import Text.ParserCombinators.Parsec

data JSONValue = JNumber Double
               | JBool Bool
               | JString String
               | JArray [JSONValue]
               | JObject [(String, JSONValue)]
               | JNull
  deriving (Eq, Ord, Show)


parseNumbers :: Parser JSONValue
parseNumbers = fmap JNumber (lexme numbers)

parseNull :: Parser JSONValue
parseNull = lexme matchNull *> pure JNull

parseBool :: Parser JSONValue
parseBool = fmap JBool (lexme bool)

parseStrings :: Parser JSONValue
parseStrings = fmap JString (lexme strings)

parseArray :: Parser [JSONValue]
parseArray = do
  ws
  arr <- char '[' *> (jsonValue `sepBy` char ',') <* char ']'
  ws
  return arr

objectEntry = do
  ws
  key <- strings
  char ':'
  value <- jsonValue
  ws
  return (key, value)

parseObject :: Parser [(String, JSONValue)]
parseObject = char '{' *> (objectEntry `sepBy` char ',') <* char '}'

jsonValue :: Parser JSONValue
jsonValue = lexme (parseNull
        <|> parseBool
        <|> parseStrings
        <|> fmap JArray parseArray
        <|> fmap JObject parseObject
        <|> parseNumbers -- throws exception for no parse on invalid json
        <?> "invalid json found")

getJSONFromFile :: String -> IO (Either ParseError JSONValue)
getJSONFromFile file = do
  text <- readFile file
  print "starting parse"
  return (parse jsonValue file text)