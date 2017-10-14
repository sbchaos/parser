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
parseNumbers = fmap JNumber numbers

parseNull :: Parser JSONValue
parseNull = matchNull *> pure JNull

parseBool :: Parser JSONValue
parseBool = fmap JBool bool

parseStrings :: Parser JSONValue
parseStrings = fmap JString strings

parseArray :: Parser [JSONValue]
parseArray = char '[' *> (jsonValue `sepBy` char ',') <* char ']'

objectEntry = do
  key <- strings
  char ':'
  value <- jsonValue
  return (key, value)

parseObject :: Parser [(String, JSONValue)]
parseObject = char '{' *> (objectEntry `sepBy` char ',') <* char '}'

jsonValue :: Parser JSONValue
jsonValue = parseNull
        <|> parseBool
        <|> parseStrings
        <|> fmap JArray parseArray
        <|> fmap JObject parseObject
        <|> parseNumbers -- throws exception for no parse on invalid json
        <?> "invalid json found"
