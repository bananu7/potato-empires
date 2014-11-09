{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}

module Main where
import Test.Hspec

import Potato.Game
import Control.Lens hiding (index)
import Control.Monad.State

spec :: Spec
spec = do
    describe "battle" $ do
        it "subtracts units' battleValues and picks the greater" $ do
            battle (Unit 10 Redosia) (Unit 5 Shitloadnam) `shouldBe` (Unit 5 Redosia)
        it "properly acts when forces are equal, preferring attacker" $ do
            battle (Unit 10 Redosia) (Unit 10 Shitloadnam) `shouldBe` (Unit 1 Redosia)

    describe "conquering cities" $ do
        it "lost battle should not change city ownership" $ do
            let initialState = createGameState $ emptyMap
                              & (ix (Point 0 0).unit) `set` (Just $ Unit 12 Shitloadnam)
                              & (ix (Point 0 0).city) `set` (Just $ City "Red city" (Just Shitloadnam))
                              & (ix (Point 1 0).unit) `set` (Just $ Unit 10 Redosia)

                act = move Redosia $ Move (Point 1 0) (Point 0 0)
            (execState act initialState) ^? (gameMap . ix (Point 0 0). city . traverse . conqueror . traverse) `shouldBe` (Just Shitloadnam)



    describe "game over: " $ do
        let initialState = createGameState $ emptyMap
                              & (ix (Point 0 0).unit) `set` (Just $ Unit 12 Redosia)
                              & (ix (Point 0 0).city) `set` (Just $ City "Red city" (Just Redosia))
                              & (ix (Point 1 0).city) `set` (Just $ City "Shit city" (Just Shitloadnam))
            act = move Redosia $ Move (Point 0 0) (Point 1 0)
        it "after final move victor should be the current player" $ do
            (execState act initialState) ^. currentPlayer `shouldBe` Redosia
        it "final move's result should be game over" $ do
            (evalState act initialState)  `shouldBe` GameOver

    describe "move validation: " $ do
        it "should reject move outside of map's boundaries" $ do
            let initialState = createGameState $ emptyMap
                                  & (ix (Point 0 0).unit) `set` (Just $ Unit 12 Redosia)
                act = move Redosia $ Move (Point 0 0) (Point (negate 1) 0)
                in (evalState act initialState) `shouldBe` InvalidMove

        it "should reject move of other player's unit" $ do
            let initialState = createGameState $ emptyMap
                                  & (ix (Point 0 0).unit) `set` (Just $ Unit 12 Shitloadnam)
                act = move Redosia $ Move (Point 0 0) (Point 1 0)
                in (evalState act initialState) `shouldBe` InvalidMove

        it "should reject move if it's other player's turn" $ do
            let initialState = createGameState $ emptyMap
                                  & (ix (Point 0 0).unit) `set` (Just $ Unit 12 Shitloadnam)
                act = move Shitloadnam $ Move (Point 0 0) (Point 1 0)
                in (evalState act initialState) `shouldBe` InvalidMove

        it "should reject move if there's no unit on 'from' field" $ do
            let initialState = createGameState emptyMap
                act = move Redosia $ Move (Point 0 0) (Point 1 0)
                in (evalState act initialState) `shouldBe` InvalidMove

                

main :: IO ()
main = hspec spec