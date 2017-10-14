{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module JSON.Parser.Number(
  number
) where

import Control.Monad
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>))

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

digits = many1 digit

plus = char '+' *> digits

minus = char '-' <:> digits

integer = plus <|> minus <|> digits

number :: Parser Double
number = fmap rd $ integer <++> decimal <++> exponent
    where rd       = read :: String -> Double
          decimal  = option "" $ char '.' <:> digits
          exponent = option "" $ oneOf "eE" <:> integer
