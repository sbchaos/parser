module JSON.Parser.Parser(
  bool,
  ws,
  lexme,
  matchNull,
  strings,
  numbers
) where

import Text.ParserCombinators.Parsec

-- | Whitespace removing parser
ws :: Parser String
ws = many (oneOf " \t\n")

lexme :: Parser a -> Parser a
lexme p = ws *> p <* ws

-- | null parser
matchNull :: Parser String
matchNull = string "null"

-- | The boolean parser
matchTrue :: Parser Bool
matchTrue = string "true" *> pure True

matchFalse :: Parser Bool
matchFalse = string "false" *> pure False

-- The alternative between true and false
bool :: Parser Bool
bool = lexme (matchTrue <|> matchFalse)


-- | The string parser
strings :: Parser String
strings = char '"' *> many (noneOf ['"']) <* char '"'


-- | The number parser - old way throws errors
numbers :: Parser Double
numbers = do
    digits <- many (digit <|> oneOf ".-")
    return (read digits)