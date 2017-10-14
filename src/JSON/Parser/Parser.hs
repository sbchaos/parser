module JSON.Parser.Parser where

import Text.ParserCombinators.Parsec

ws :: Parser String
ws = many (oneOf " \t\n")

lexme :: Parser a -> Parser a
lexme p = ws *> p

matchTrue :: Parser Bool
matchTrue = string "true" *> pure True

matchFalse :: Parser Bool
matchFalse = string "false" *> pure False