{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (parserSpec) where

import Test.Hspec 
import Text.Megaparsec
import Parser
import Expression

parserSpec :: Spec
parserSpec = do
    describe "parseLiteralNum" $ do
        it "parses floats" $ do
            runParser parseLiteralNum "in module" "3" `shouldBe` Right (Literal "Float" "3.0")
            runParser parseLiteralNum "in module" "7.189" `shouldBe` Right (Literal "Float" "7.189")
    describe "parseLiteralBool" $ do
        it "parses bools" $ do
            runParser parseLiteralBool "in module" "True" `shouldBe` Right (Literal "Bool" "True")
            runParser parseLiteralBool "in module" "False" `shouldBe` Right (Literal "Bool" "False")
    describe "parseApp" $ do
        it "parses apps" $ do
            runParser parseApp "in module" "x y" `shouldBe` Right (App (Var "x") (Var "y"))
            runParser parseApp "in module" "x y z w" `shouldBe` Right (App (App (App (Var "x") (Var "y")) (Var "z")) (Var "w"))
            runParser parseApp "in module" "x (y (z w))" `shouldBe` Right (App (Var "x") (App (Var "y") (App (Var "z") (Var "w"))))
            runParser parseApp "in module" "(x y) z" `shouldBe` Right (App (App (Var "x") (Var "y")) (Var "z"))
            runParser parseApp "in module" "x ((y z) w)" `shouldBe` Right (App (Var "x") (App (App (Var "y") (Var "z")) (Var "w")))
    describe "parseOp" $ do
        it "parses ops" $ do
            runParser parseOp "in module" "3 + x * y" `shouldBe` Right (App (App (Var "+") (Literal "Float" "3.0")) (App (App (Var "*") (Var "x")) (Var "y")))
            runParser parseOp "in module" "(x) + 3" `shouldBe` Right (App (App (Var "+") (Var "x")) (Literal "Float" "3.0"))
            runParser parseOp "in module" "(x - 3) * (y 4)" `shouldBe`Right (App (App (Var "*") (App (App (Var "-") (Var "x")) (Literal "Float" "3.0"))) (App (Var "y") (Literal "Float" "4.0")) )



            
            
            