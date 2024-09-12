module Main where

import Test.Hspec
import ParserSpec (parserSpec)

main :: IO ()
main = do
    hspec parserSpec