module Main where
import Test.Hspec

import Potato.Game

spec :: Spec
spec = do
    describe "battle" $ do
        it "subtracts units' battleValues and picks the greater" $ do
            battle (Unit 10 Redosia) (Unit 5 Shitloadnam) `shouldBe` (Unit 5 Redosia)
        it "properly acts when forces are equal, preferring attacker" $ do
            battle (Unit 10 Redosia) (Unit 10 Shitloadnam) `shouldBe` (Unit 1 Redosia)
            
main :: IO ()
main = hspec spec
