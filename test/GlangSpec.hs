module GlangSpec (glangSpec) where

import Test.Hspec
import Glang
import Gmachine
import Expression

glangSpec :: Spec
glangSpec = do
    describe "evalToOutputFromFile" $ do
        it "executes evalToOutput with a file as an input" $ do
            evalToOutputFromFile "test/example_prog/prog1.g" `shouldBe` Right (LitNode (Literal "Float" "4.0"))
            evalToOutputFromFile "test/example_prog/prog2.g" `shouldBe` Right (LitNode (Literal "Float" "23.0"))
            evalToOutputFromFile "test/example_prog/prog3.g" `shouldBe` Right (LitNode (Literal "Float" "5.0"))
            evalToOutputFromFile "test/example_prog/prog4.g" `shouldBe` Right (LitNode (Literal "Float" "41.0"))