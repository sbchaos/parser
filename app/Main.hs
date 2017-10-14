module Main where

import JSON.JSON

main :: IO ()
main = do
  name <- getLine
  json <- getJSONFromFile name
  print json
