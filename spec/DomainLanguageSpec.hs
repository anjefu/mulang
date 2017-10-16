module DomainLanguageSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast
import           Language.Mulang.DomainLanguage (DomainLanguage(..), hasMisspelledIdentifiers, hasTooShortIdentifiers, hasWrongCaseIdentifiers)
import           Language.Mulang.Parsers.Haskell (hs)
import           Language.Mulang.Parsers.JavaScript (js)
import           Text.Dictionary (toDictionary)
import           Text.Inflections.Tokenizer (camelCase, rubyCase)

spec :: Spec
spec = do
  let english = toDictionary ["a","day","great","is","today"]
  let jargon  = ["ui", "js"]
  let language = DomainLanguage english camelCase 3 jargon

  describe "hasTooShortIdentifiers" $ do
    let run = hasTooShortIdentifiers language

    it "is True when it is a one-char identifier" $ do
      run (hs "x = True") `shouldBe` True

    it "is True when it has numbers" $ do
      run (hs "x1 = True") `shouldBe` True

    it "is True when it is a two-chars identifier" $ do
      run (hs "xy = True") `shouldBe` True

    it "is False when it is a three-chars identifier" $ do
      run (hs "zip = []") `shouldBe` False

    it "is False when it contains a short parameter name"  $ do
      run (hs "aFunction a = a") `shouldBe` False

    it "is True when it contains a short local variable name" $ do
      run (js "function foo() { var x = 1; return x }") `shouldBe` True

    it "is False when it uses a short named function" $ do
      run (hs "aFunction aNumber = f aNumber") `shouldBe` False

    it "is False when it contains a short local variable name in a method" $ do
      run (js "var pepita = {come:function(){var x = 1; }, vola:function(){}};") `shouldBe` True

    it "is False when it contains a short local parameter name in a method" $ do
      run (js "var pepita = {come:function(x){ }, vola:function(){}};") `shouldBe` False

    it "is True when it contains a short named method" $ do
      run (js "var pepita = {x:function(){}, vola:function(){}};") `shouldBe` True

    it "is True when it contains a short named attribute" $ do
      run (js "var pepita = {x: 2, vola:function(){}};") `shouldBe` True

    it "is True when it contains a short variable name" $ do
      run (js "var x = 3;") `shouldBe` True

    it "is False when it is jargon" $ do
      run (hs "ui = False") `shouldBe` False

  describe "hasWrongCaseIdentifiers" $ do
    context "camelCase language" $ do
      let run = hasWrongCaseIdentifiers language

      it "is True when there are snake case identifier on a camel case language" $ do
        run (js "var a_day = 'monday'") `shouldBe` True

      it "is False when there are only camel case identifier on a camel case language" $ do
        run (js "var aDay = 'monday'") `shouldBe` False

      it "is False when it has numbers but proper casing" $ do
        run (js "var aFoo2 = 'monday'") `shouldBe` False

    context "rubyCase language" $ do
      let run = hasWrongCaseIdentifiers (DomainLanguage english rubyCase 3 jargon)

      it "is True when there are lower camel case identifier" $ do
        run (Variable "helloWorld" MuNull) `shouldBe` True

      it "is False when there are upper camel case identifier" $ do
        run (Variable "HelloWorld" MuNull) `shouldBe` False

      it "is False when there are lower snake case identifier" $ do
        run (Variable "hello_world" MuNull) `shouldBe` False

      it "is True when there are upper snake case identifier" $ do
        run (Variable "Hello_World" MuNull) `shouldBe` True


  describe "hasMisspelledIdentifiers" $ do
    let run = hasMisspelledIdentifiers language

    it "is False when it is a single, well written token" $ do
      run (hs "today = True") `shouldBe` False

    it "is False when it is a single, well written but jargon token" $ do
      run (hs "js = True") `shouldBe` False

    it "is False when all tokens are well-written" $ do
      run (hs "todayIsAGreatDay = True") `shouldBe` False

    it "is True when it is a single, bad written token" $ do
      run (hs "tuday = True") `shouldBe` True

    it "is False when there are typos" $ do
      run (hs "tudayIsAGreatDay = True") `shouldBe` True
      run (hs "todayIsAGraetDay = True") `shouldBe` True
