module Main where

import Test.Hspec
import ParserSpec (parserSpec)
import GlangSpec (glangSpec)

main :: IO ()
main = do
    hspec parserSpec
    hspec glangSpec