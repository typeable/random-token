module TokenSpec where

import Random.Token
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  prop "asciiToToken . tokenToAscii ~ id" $ \ (t :: Token ()) ->
    t == (either (error . show) id $ asciiToToken $ tokenToAscii t)
  prop "textToToken . tokenToText ~ id" $ \ (t :: Token ()) ->
    t == (either (error . show) id $ textToToken $ tokenToText t)
