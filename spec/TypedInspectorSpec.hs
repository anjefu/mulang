module TypedInspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector.Typed
import           Language.Mulang.Identifier (named)
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Parsers.Java (java)

spec :: Spec
spec = do
  describe "typesReturnAs" $ do
    it "is True when typed, in hs" $ do
      typesReturnAs (named "Int") (hs "f :: Int\nf = 4")  `shouldBe` True

    it "is False when not explicitly typed, in hs" $ do
      typesReturnAs (named "Int") (hs "f = 4")  `shouldBe` False

    it "is True when typed, in java" $ do
      typesReturnAs (named "int") (java "class Foo { int f() { return 4; } }")  `shouldBe` True

    it "is False when typed with another type, in java" $ do
      typesReturnAs (named "double") (java "class Foo { int f() { return 4; } }")  `shouldBe` False

  describe "typesParameterAs" $ do
    it "is True when typed, in hs" $ do
      typesParameterAs (named "Int") (hs "f :: Int -> Bool\nf n = n > 4 ")  `shouldBe` True

    it "is False when not explicitly typed, in hs" $ do
      typesParameterAs (named "Int") (hs "f n = n > 4 ")  `shouldBe` False

    it "is True when typed, in java" $ do
      typesParameterAs (named "int") (java "class Foo { bool f(int n) { return n > 4; } }")  `shouldBe` True

    it "is False when typed with another type, in java" $ do
      typesParameterAs (named "int") (java "class Foo { bool f(double n) { return n > 4; } }")  `shouldBe` False

  describe "declaresTypeSignature" $ do
    it "is True when type signature is present" $ do
      declaresTypeSignature (named "x") (hs "x :: Int\n\
                           \x = 3") `shouldBe` True

    it "is True when type signature just signature is present" $ do
      declaresTypeSignature (named "x") (hs "x :: Int") `shouldBe` True

    it "is False when type signature is absent " $ do
      declaresTypeSignature (named "x") (hs "x = 2") `shouldBe` False
